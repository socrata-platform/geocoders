package com.socrata.geocoders

import java.io.IOException

import com.rojoma.json.v3.ast._
import com.rojoma.json.v3.io.{CompactJsonWriter, JsonReader}
import com.rojoma.json.v3.matcher._
import com.socrata.http.client.{SimpleHttpRequest, RequestBuilder, HttpClient}

import scala.concurrent.duration.FiniteDuration

case class EsriRest(referer: String,
                    tokenHost: String,
                    host: String,
                    username: String,
                    password: String,
                    tokenExpiration: FiniteDuration,
                    expirationBufferMS: Long,
                    requestTimeoutMS: Int)

class EsriRestGeocoder(http: HttpClient, esriRest: EsriRest, metricProvider: (GeocodingResult, Long) => Unit, val retryCount: Int = 5) extends BaseGeocoder with RetryWithLogging {
  val provider = "ESRI"
  val log = org.slf4j.LoggerFactory.getLogger(classOf[EsriRestGeocoder])
  val serviceDescription = RequestBuilder(esriRest.host, secure = true).p("arcgis", "rest", "services", "World", "GeocodeServer").q("f" -> "json")
  val geocodingService = RequestBuilder(esriRest.host, secure = true).p("arcgis", "rest", "services", "World", "GeocodeServer", "geocodeAddresses")
  val tokenService = RequestBuilder(esriRest.tokenHost, secure = true).p("sharing", "generateToken")

  val tokenExpirationInMinutes = esriRest.tokenExpiration.toMinutes
  private var cachedToken: String = _
  private var cachedTokenExpires = Long.MinValue

  val outSR = "4326"

  override def batchSize: Int =
    doGet(serviceDescription).dyn.locatorProperties.SuggestedBatchSize.? match {
      case Right(n: JNumber) =>
        n.toInt
      case Right(_) =>
        throw new GeocodingFailure("Unable to determine geocoding batch size: not a number at locatorProperties.SuggestedBatchSize")
      case Left(err) =>
        throw new GeocodingFailure("Unable to determine geocoding batch size: " + err.english)
    }

  override def geocode(addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] =
    addresses.grouped(batchSize).flatMap(geocodeBatch(metricProvider(_, _), _)).toVector

  def doGet(req: RequestBuilder): JValue =
    doReq(req, _.get)

  def doPost(req: RequestBuilder, body: Map[String, String]): JValue =
    doReq(req, _.form(body))

  def doReq(req: RequestBuilder, finish: RequestBuilder => SimpleHttpRequest): JValue = {
    retrying[JValue] {
      http.execute(finish(req.timeoutMS(esriRest.requestTimeoutMS))).run { resp =>
        resp.resultCode match {
          case 200 =>
            // ESRI likes to return things as text/plain
            JsonReader.fromReader(resp.reader())
          case 403 =>
            log.info("403 from ESRI!")
            credentialsException("received 403 from ESRI!")
          case 401 =>
            log.info("401 from ESRI!  I'm going to retry just in case the token expired, but I'm not hopeful.")
            throw new IOException("Received result code 401 from ESRI")
          case other =>
            throw new IOException(s"Unexpected result code $other from ESRI")
        }
      }
    }
  }

  private def renewToken() {
    // http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#//02r3000000n3000000

    val body = Map(
      "username" -> esriRest.username,
      "password" -> esriRest.password,
      "referer" -> esriRest.referer,
      "expiration" -> (tokenExpirationInMinutes + esriRest.expirationBufferMS).max(Int.MaxValue).toInt.toString,
      "f" -> "json"
    )

    val json = doPost(tokenService, body)

    val tokenVar = Variable[String]()
    val expiresVar = Variable[Long]()
    val Pattern = PObject(
      "token" -> tokenVar,
      "expires" -> expiresVar
    )

    json match {
      case Pattern(res) =>
        cachedTokenExpires = expiresVar(res) - esriRest.expirationBufferMS
        cachedToken = tokenVar(res)
      case other =>
        fail("Didn't get a valid response from token request: " + other)
    }
  }

  def token = synchronized {
    if (cachedTokenExpires <= System.currentTimeMillis()) renewToken()
    cachedToken
  }

  def encodeForEsri(addr: InternationalAddress): Map[String, JValue] = {
    val InternationalAddress(address, locality, subregion, region, postalCode, _) = addr
    val mb = Map.newBuilder[String, String]
    address.foreach(mb += "Address" -> _)
    locality.foreach(mb += "City" -> _)
    subregion.foreach(mb += "Subregion" -> _)
    region.foreach(mb += "Region" -> _)
    postalCode.foreach(mb += "Postal" -> _)
    mb.result().mapValues(JString(_))
  }

  val resultIdVar = Variable[Int]()
  val longitudeVar = Variable[JNumber]()
  val latitudeVar = Variable[JNumber]()
  val GoodResult = PObject(
    "attributes" -> PObject(
      "ResultID" -> resultIdVar,
      "Status" -> FirstOf("M", "T")
    ),
    "location" -> PObject(
      "x" -> longitudeVar,
      "y" -> latitudeVar
    )
  )
  val BadResult = PObject(
    "attributes" -> PObject(
      "ResultID" -> resultIdVar,
      "Status" -> "U"
    )
  )

  case class DecodedResult(id: Int, latLon: Option[LatLon])

  def decodeResult(metric: (GeocodingResult, Int) => Unit, fromEsri: JValue): DecodedResult = fromEsri match {
    case GoodResult(res) =>
      metric(SuccessResult, 1)
      DecodedResult(resultIdVar(res), Some(LatLon(latitudeVar(res).toDouble, longitudeVar(res).toDouble)))
    case BadResult(res) =>
      metric(InsufficientlyPreciseResult, 1)
      DecodedResult(resultIdVar(res), None)
    case other =>
      metric(UninterpretableResult, 1)
      fail("Unable to interpret geocoding result from esri: " + fromEsri)
  }

  private def geocodeBatch(metric: (GeocodingResult, Int) => Unit, addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] = {
    if (addresses.isEmpty) return Seq.empty
    val sourceCountry = addresses.head.country

    // ok, caching and deduping and etc are all handled in other classes, so let's just naïvely geocode these things.
    val asObject = JObject(Map(
      "records" -> JArray(addresses.view.zipWithIndex.map { case (addr, idx) =>
        JObject(Map(
          "attributes" -> JObject(encodeForEsri(addr) + ("OBJECTID" -> JNumber(idx)))))
      })))

    val body = Map(
      "addresses" -> CompactJsonWriter.toString(asObject),
      "sourceCountry" -> sourceCountry,
      "outSR" -> outSR,
      "token" -> token,
      "f" -> "json"
    )

    val result = doPost(geocodingService, body)
    result.dyn.locations.? match {
      case Right(JArray(locations)) =>
        val res = locations.map(decodeResult(metric, _)).sortBy(_.id)
        if (res.length != addresses.length) fail("Wrong number of results from ESRI; expected " + addresses.length + " but got " + res.length)
        log.info("Geocoded {} addresses", res.length)
        res.map { result => (result.latLon, JNull)}
      case Right(_) =>
        fail("`locations' field found but it wasn't an array!")
      case Left(err) =>
        fail("No `locations' field found: " + err.english)
    }
  }
}
