package com.socrata.geocoders

import com.rojoma.json.v3.ast.{JArray, JObject, JValue, JNumber, JNull}
import com.rojoma.json.v3.io.{CompactJsonWriter, JsonReader}
import com.rojoma.json.v3.interpolation._
import com.rojoma.json.v3.matcher._
import com.socrata.http.client.{SimpleHttpRequest, RequestBuilder, HttpClient}
import com.rojoma.simplearm.v2.conversions._
import com.rojoma.json.v3.conversions._

import scala.concurrent.duration.FiniteDuration

class EsriGeocoder(http: HttpClient, tokenHost: String, host: String, username: String, password: String, tokenExpiration: FiniteDuration, metricProvider: (GeocodingResult, Long) => Unit) extends BaseGeocoder with Sourcable {
  val log = org.slf4j.LoggerFactory.getLogger(classOf[EsriGeocoder])

  val source = caching.Source("esri")

  val serviceDescription = RequestBuilder(host, secure = true).p("arcgis","rest","services","World","GeocodeServer").q("f" -> "json")
  val geocodingService = RequestBuilder(host, secure = true).p("arcgis","rest","services","World","GeocodeServer","geocodeAddresses")
  val tokenService = RequestBuilder(tokenHost, secure = true).p("sharing","generateToken")
  val requestTimeoutMS = 30000

  val tokenExpirationInMinutes = tokenExpiration.toMinutes
  val referer = "http://socrata.com/"
  private var cachedToken: String = _
  private var cachedTokenExpires = Long.MinValue
  private val expirationBufferMS = 60000L

  val outSR = "4326"

  def doGet(req: RequestBuilder): JValue =
    doReq(req, _.get)

  def doPost(req: RequestBuilder, body: Map[String, String]): JValue =
    doReq(req, _.form(body))

  def fail(cause: String): Nothing = {
    log.error(cause)
    throw new GeocodingFailure(cause)
  }

  def doReq(req: RequestBuilder, finish: RequestBuilder => SimpleHttpRequest): JValue = {
    def loop(retriesRemaining: Int): JValue = {
      if(retriesRemaining == 0) fail("Ran out of retries")

      sealed abstract class Result
      case object Retry extends Result
      case class Fail(cause: String) extends Result
      case class Success(v: JValue) extends Result

      val res = try {
        http.execute(finish(req.timeoutMS(requestTimeoutMS))).run { resp =>
          resp.resultCode match {
            case 200 =>
              // ESRI likes to return things as text/plain
              Success(JsonReader.fromReader(resp.reader()))
            case 403 =>
              log.info("403 from ESRI!")
              Fail("Auth failure")
            case 401 =>
              log.info("401 from ESRI!  I'm going to retry just in case the token expired, but I'm not hopeful.")
              Retry
            case other =>
              log.info("Unexpected result code {} from ESRI!  Retrying...", other)
              Retry
          }
        }
      } catch {
        case e: Exception =>
          log.info("Unexpected exception while talking to ESRI; retrying request", e)
          Retry
      }
      res match {
        case Success(v) => v
        case Retry => loop(retriesRemaining - 1)
        case Fail(cause) => fail(cause)
      }
    }
    loop(5)
  }

  private def renewToken() {
    // http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#//02r3000000n3000000

    val body = Map(
      "username" -> username,
      "password" -> password,
      "referer" -> referer,
      "expiration" -> (tokenExpirationInMinutes + expirationBufferMS).max(Int.MaxValue).toInt.toString,
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
        cachedTokenExpires = expiresVar(res) - expirationBufferMS
        cachedToken = tokenVar(res)
      case other =>
        fail("Didn't get a valid response from token request: " + other)
    }
  }

  def token = synchronized {
    if(cachedTokenExpires <= System.currentTimeMillis()) renewToken()
    cachedToken
  }

  val batchSize: Int =
    doGet(serviceDescription).dyn.locatorProperties.SuggestedBatchSize.? match {
      case Right(n: JNumber) =>
        n.toInt
      case Right(_) =>
        throw new Exception("Unable to determine geocoding batch size: not a number at locatorProperties.SuggestedBatchSize")
      case Left(err) =>
        throw new Exception("Unable to determine geocoding batch size: " + err.english)
    }

  val addressVar = Variable[String]()
  val cityVar = Variable[String]()
  val stateVar = Variable[String]()
  val zipVar = Variable[String]()
  val addressPattern = PObject(
    "Address" -> POption(addressVar),
    "City" -> POption(cityVar),
    "Region" -> POption(stateVar),
    "Postal" -> POption(zipVar)
  )

  def encodeAddress(a: InternationalAddress): JObject = {
    // Hm, I should really make the return type of this inferrable.
    addressPattern.generate(addressVar :=? a.address, cityVar :=? a.locality, stateVar :=? a.region, zipVar :=? a.postalCode).asInstanceOf[JObject]
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

  sealed abstract class DecodedAddress {
    val id: Int
  }
  case class SuccessfullyGeocoded(id: Int, lat: JNumber, lon: JNumber) extends DecodedAddress
  case class NoGeocode(id: Int) extends DecodedAddress

  def decodeAddress(fromEsri: JValue): DecodedAddress = fromEsri match {
    case GoodResult(res) =>
      metricProvider(SuccessResult, 1)
      SuccessfullyGeocoded(resultIdVar(res), latitudeVar(res), longitudeVar(res))
    case BadResult(res) =>
      metricProvider(NoResult, 1)
      NoGeocode(resultIdVar(res))
    case other =>
      metricProvider(UninterpretableResult, 1)
      fail("Unable to interpret geocoding result from esri: " + fromEsri)
  }

  def geocode(addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] = {
    val byCountry = addresses.zipWithIndex.groupBy(_._1.country).mapValues(_.toVector)

    byCountry.flatMap { case (country, addressesAndIndexes) =>
      // ok, caching and deduping and etc are all handled in other classes, so let's just naïvely geocode these things.
      val asObject =
        JObject(
          Map(
            "records" -> JArray(
              addressesAndIndexes.map { case (addr, idx) =>
                JObject(
                  Map(
                    "attributes" -> JObject(encodeAddress(addr).fields + ("OBJECTID" -> JNumber(idx)))))
              })))

      val body = Map(
        "addresses" -> CompactJsonWriter.toString(asObject),
        "sourceCountry" -> country,
        "outSR" -> outSR,
        "token" -> token,
        "f" -> "json"
      )

      val result = doPost(geocodingService, body)
      result.dyn.locations.? match {
        case Right(JArray(locations)) =>
          val res = locations.map(decodeAddress).sortBy(_.id)
          if(res.length != addresses.length) fail("Wrong number of results from ESRI; expected " + addresses.length + " but got " + res.length)
          log.info("Geocoded {} addresses", res.length)
          (addressesAndIndexes, res).zipped.map { (addrIdx, res) =>
            val (addr, idx) = addrIdx
            if(idx != res.id) fail("Mismatched object IDs from esri; expected " + idx + " but got " + res.id)
            res match {
              case SuccessfullyGeocoded(_, lat, lon) => (idx, Some(LatLon(lat = lat.toDouble, lon = lon.toDouble)), JNull)
              case NoGeocode(_) => (idx, None, JNull)
            }
          }
        case Right(_) =>
          fail("`locations' field found but it wasn't an array!")
        case Left(err) =>
          fail("No `locations' field found: " + err.english)
      }
    }.toVector.sortBy(_._1).map { case (_, pt, annotation) => (pt, annotation) }
  }
}
