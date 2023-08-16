package com.socrata.geocoders

import com.rojoma.json.v3.ast.{JArray, JObject, JValue, JNumber, JNull}
import com.rojoma.json.v3.codec.{JsonDecode, DecodeError}
import com.rojoma.json.v3.io.{CompactJsonWriter, JsonReader}
import com.rojoma.json.v3.interpolation._
import com.rojoma.json.v3.matcher._
import com.rojoma.json.v3.util.AutomaticJsonDecodeBuilder
import com.socrata.http.client.{SimpleHttpRequest, RequestBuilder, HttpClient}

import scala.concurrent.duration.FiniteDuration

class EsriGeocoder(
  http: HttpClient,
  apiToken: String,
  host: String, // probably "geocode.arcgis.com"
  batchSizeExpiration: FiniteDuration,
  metricProvider: (GeocodingResult, Long) => Unit,
  retryCount: Int = 5
) extends BaseGeocoder {
  val log = org.slf4j.LoggerFactory.getLogger(classOf[EsriGeocoder])

  val serviceDescription = RequestBuilder(host, secure = true).p("arcgis","rest","services","World","GeocodeServer").q("f" -> "json")
  val geocodingService = RequestBuilder(host, secure = true).p("arcgis","rest","services","World","GeocodeServer","geocodeAddresses")
  val requestTimeoutMS = 30000

  val batchSizeExpirationInMinutes = batchSizeExpiration.toMinutes
  private var cachedBatchSize: Int = _
  private var cachedBatchSizeExpires = Long.MinValue

  val outSR = "4326"

  override def geocode(addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] =
    addresses.grouped(batchSize).flatMap(geocodeBatch(metricProvider, _)).toVector

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
    loop(retryCount)
  }

  private def renewBatchSize() {
    cachedBatchSize =
      doGet(serviceDescription).dyn.locatorProperties.SuggestedBatchSize.? match {
        case Right(n: JNumber) =>
          n.toInt
        case Right(_) =>
          throw new Exception("Unable to determine geocoding batch size: not a number at locatorProperties.SuggestedBatchSize")
        case Left(err) =>
          throw new Exception("Unable to determine geocoding batch size: " + err.english)
      }
    cachedBatchSizeExpires = System.currentTimeMillis() + batchSizeExpirationInMinutes*60000L
  }

  override def batchSize = synchronized {
    if(cachedBatchSizeExpires <= System.currentTimeMillis()) renewBatchSize()
    cachedBatchSize
  }

  val addressVar = Variable[String]()
  val cityVar = Variable[String]()
  val stateVar = Variable[String]()
  val subregionVar = Variable[String]()
  val zipVar = Variable[String]()
  val countryVar = Variable[String]()
  val addressPattern = PObject(
    "Address" -> POption(addressVar),
    "City" -> POption(cityVar),
    "Region" -> POption(stateVar),
    "Subregion" -> POption(subregionVar),
    "Postal" -> POption(zipVar),
    "Country" -> POption(countryVar)
  )

  def encodeAddress(addr: InternationalAddress, singleCountry: Boolean): JObject = {
    val InternationalAddress(address, locality, subregion, region, postalCode, country) = addr
    addressPattern.generate(
      addressVar :=? address,
      cityVar :=? locality,
      stateVar :=? region,
      subregionVar :=? subregion,
      zipVar :=? postalCode,
      countryVar :=? (if(singleCountry) None else Some(country))
    ).asInstanceOf[JObject]
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

  def decodeAddress(metric: (GeocodingResult, Long) => Unit, fromEsri: JValue): DecodedAddress = fromEsri match {
    case GoodResult(res) =>
      metric(SuccessResult, 1)
      SuccessfullyGeocoded(resultIdVar(res), latitudeVar(res), longitudeVar(res))
    case BadResult(res) =>
      metric(BadInputResult, 1)
      NoGeocode(resultIdVar(res))
    case other =>
      metric(UninterpretableResult, 1)
      fail("Unable to interpret geocoding result from esri: " + fromEsri)
  }

  private def attemptToDecodeError(value: JValue): Option[String] = {
    case class Error(message: String, details: Option[Seq[String]])
    case class Envelope(error: Error)

    implicit val errDec = AutomaticJsonDecodeBuilder[Error]
    implicit val envDec = AutomaticJsonDecodeBuilder[Envelope]

    JsonDecode.fromJValue[Envelope](value) match {
      case Right(Envelope(Error(message, details))) =>
        val assembledMessage =
          message + details.filterNot(_.isEmpty).fold("") { details =>
            ":\n" + details.map("  " + _).mkString("\n")
          }
        Some(assembledMessage)
      case Left(_) =>
        None
    }
  }

  def geocodeBatch(metric: (GeocodingResult, Long) => Unit, addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] = {
    if(addresses.isEmpty) return Nil

    // ESRI docs say geocoding can be more efficient if we factor out
    // the country when all addresses in a batch share that country.
    val singleCountry = addresses.tail.forall(_.country == addresses.head.country)

    // ok, caching and deduping and etc are all handled in other classes, so let's just naïvely geocode these things.
    val asObject = JObject(Map(
      "records" -> JArray(addresses.view.zipWithIndex.map { case (addr, idx) =>
          JObject(Map(
            "attributes" -> JObject(encodeAddress(addr, singleCountry).fields + ("OBJECTID" -> JNumber(idx)))))
      })))

    var body = Map(
      "addresses" -> CompactJsonWriter.toString(asObject),
      "outSR" -> outSR,
      "token" -> apiToken,
      "f" -> "json"
    )

    if(singleCountry) {
      body += "sourceCountry" -> addresses.head.country
    }

    val result = doPost(geocodingService, body)
    result.dyn.locations.? match {
      case Right(JArray(locations)) =>
        val res = locations.map(decodeAddress(metric, _)).sortBy(_.id)
        if(res.length != addresses.length) fail("Wrong number of results from ESRI; expected " + addresses.length + " but got " + res.length)
        log.info("Geocoded {} addresses", res.length)
        (addresses, res).zipped.map { (addr, res) =>
          res match {
            case SuccessfullyGeocoded(_, lat, lon) => (Some(LatLon(lat.toDouble, lon.toDouble)), JNull)
            case NoGeocode(_) => (None, JNull)
          }
        }
      case Right(_) =>
        fail("`locations' field found but it wasn't an array!")
      case Left(err) =>
        attemptToDecodeError(result) match {
          case Some(err) =>
            fail("Error from ESRI: " + err)
          case None =>
            log.error("Bad response: {}", result)
            fail("No `locations' field found: " + err.english)
        }
    }
  }
}
