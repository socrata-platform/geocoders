package com.socrata.geocoders

import java.io.{ByteArrayInputStream, InputStream, Closeable}

import com.socrata.http.client.exceptions.FullTimeout
import com.socrata.http.client.{ResponseInfo, SimpleHttpRequest, HttpClient}

class MapQuestGeocoderTest extends BaseTest {

  def mockHttpClient(expectedResultCode: Int, expectedBody: String, succeedAfter: Int) = new HttpClient {
    private var attempt = -1

    override def executeRawUnmanaged(req: SimpleHttpRequest): RawResponse with Closeable = {
      attempt += 1
      if (attempt < succeedAfter) throw new FullTimeout
      rawResponse
    }

    private val rawResponse = new RawResponse with Closeable {
      override val responseInfo: ResponseInfo = new ResponseInfo {
        override def resultCode: Int = 200

        override def headerNames: Set[String] = Set.empty

        override def headers(name: String): Array[String] = Array("application/json")
      }
      override val body: InputStream = new ByteArrayInputStream(expectedBody.getBytes)

      override def close(): Unit = {}
    }

    override def close(): Unit = {}
  }

  def withMocks[A](expectedResultCode: Int, expectedBody: String, succeedAfter: Int = 0)(f: (GeocodeMockMetric, BaseGeocoder) => A) = {
    val httpClient = mockHttpClient(expectedResultCode, expectedBody, succeedAfter)
    val metric = new GeocodeMockMetric
    val geocoder = new MapQuestGeocoder(httpClient, "socrata-app-token", metric.increment)
    f( metric, geocoder)
  }

  val address0 = InternationalAddress(Some("55th Ave % E. 16th St"), Some(" Oakland"), None, Some("CA"), None, "US")
  val address1 = InternationalAddress(Some("3002 NW 72nd Street %%%%"), Some("Seattle"), None, Some("WA"), Some("98117"), "US")
  val address2 = InternationalAddress(Some("55th Ave & E. 16th St"), Some("Oakland"), None, Some("CA"), None, "US")

  test("Should be able to geocode after receiving a FullTimeout") {
    val responseBody = """{
                         |   "results" : [
                         |      {
                         |         "locations" : [
                         |            {
                         |               "adminArea5Type" : "City",
                         |               "type" : "s",
                         |               "adminArea1" : "US",
                         |               "geocodeQuality" : "STREET",
                         |               "linkId" : "rnr3251270",
                         |               "dragPoint" : false,
                         |               "geocodeQualityCode" : "B1CAX",
                         |               "postalCode" : "94621",
                         |               "unknownInput" : "",
                         |               "street" : "[5500 - 6299] E 16th St, 55 av",
                         |               "adminArea6" : "",
                         |               "adminArea5" : "Oakland",
                         |               "adminArea6Type" : "Neighborhood",
                         |               "sideOfStreet" : "M",
                         |               "displayLatLng" : {
                         |                  "lng" : -122.198642,
                         |                  "lat" : 37.766549
                         |               },
                         |               "latLng" : {
                         |                  "lat" : 37.766549,
                         |                  "lng" : -122.198642
                         |               },
                         |               "adminArea4Type" : "County",
                         |               "adminArea4" : "Alameda",
                         |               "adminArea3" : "CA",
                         |               "adminArea1Type" : "Country",
                         |               "adminArea3Type" : "State"
                         |            }
                         |         ],
                         |         "providedLocation" : {
                         |            "adminArea1" : "US",
                         |            "street" : "55th Ave % E. 16th St",
                         |            "adminArea3" : "CA",
                         |            "adminArea5" : " Oakland"
                         |         }
                         |      },
                         |      {
                         |         "locations" : [
                         |            {
                         |               "adminArea6Type" : "Neighborhood",
                         |               "adminArea5" : "Seattle",
                         |               "street" : "3002 NW 72nd St",
                         |               "adminArea6" : "",
                         |               "unknownInput" : "",
                         |               "postalCode" : "98117-6265",
                         |               "dragPoint" : false,
                         |               "geocodeQualityCode" : "P1AAA",
                         |               "linkId" : "r14634001|p186330168|n19828629",
                         |               "geocodeQuality" : "POINT",
                         |               "adminArea1" : "US",
                         |               "type" : "s",
                         |               "adminArea5Type" : "City",
                         |               "adminArea3Type" : "State",
                         |               "adminArea1Type" : "Country",
                         |               "adminArea3" : "WA",
                         |               "adminArea4" : "King",
                         |               "adminArea4Type" : "County",
                         |               "displayLatLng" : {
                         |                  "lng" : -122.396012,
                         |                  "lat" : 47.68115
                         |               },
                         |               "latLng" : {
                         |                  "lat" : 47.680957,
                         |                  "lng" : -122.396001
                         |               },
                         |               "sideOfStreet" : "R"
                         |            }
                         |         ],
                         |         "providedLocation" : {
                         |            "postalCode" : "98117",
                         |            "adminArea5" : "Seattle",
                         |            "adminArea1" : "US",
                         |            "street" : "3002 NW 72nd Street %%%%",
                         |            "adminArea3" : "WA"
                         |         }
                         |      },
                         |      {
                         |         "providedLocation" : {
                         |            "adminArea1" : "US",
                         |            "street" : "55th Ave & E. 16th St",
                         |            "adminArea3" : "CA",
                         |            "adminArea5" : "Oakland"
                         |         },
                         |         "locations" : [
                         |            {
                         |               "adminArea6Type" : "Neighborhood",
                         |               "adminArea5" : "Oakland",
                         |               "adminArea6" : "",
                         |               "street" : "55th Ave & E 16th St",
                         |               "unknownInput" : "",
                         |               "postalCode" : "94621",
                         |               "geocodeQualityCode" : "I1AAA",
                         |               "dragPoint" : false,
                         |               "linkId" : "245327|3251270",
                         |               "adminArea1" : "US",
                         |               "geocodeQuality" : "INTERSECTION",
                         |               "type" : "s",
                         |               "adminArea5Type" : "City",
                         |               "adminArea3Type" : "State",
                         |               "adminArea1Type" : "Country",
                         |               "adminArea3" : "CA",
                         |               "adminArea4" : "Alameda",
                         |               "adminArea4Type" : "County",
                         |               "latLng" : {
                         |                  "lat" : 37.768674,
                         |                  "lng" : -122.201465
                         |               },
                         |               "displayLatLng" : {
                         |                  "lng" : -122.201465,
                         |                  "lat" : 37.768674
                         |               },
                         |               "sideOfStreet" : "N"
                         |            }
                         |         ]
                         |      }
                         |   ],
                         |   "info" : {
                         |      "messages" : [],
                         |      "statuscode" : 0,
                         |      "copyright" : {
                         |         "text" : "� 2016 MapQuest, Inc.",
                         |         "imageUrl" : "https://api.mqcdn.com/res/mqlogo.gif",
                         |         "imageAltText" : "� 2016 MapQuest, Inc."
                         |      }
                         |   },
                         |   "options" : {
                         |      "maxResults" : 1,
                         |      "thumbMaps" : false,
                         |      "ignoreLatLngInput" : false
                         |   }
                         |}""".stripMargin
    withMocks(200, responseBody, 2) { case (metric, geocoder) =>
      val addresses = Seq(address0, address1, address2)
      val results = geocoder.geocode(addresses)
      assert(results.length == addresses.length)
    }
  }

}
