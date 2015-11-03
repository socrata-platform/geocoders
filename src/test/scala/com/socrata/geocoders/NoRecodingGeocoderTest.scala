package com.socrata.geocoders

import scala.collection.mutable.ArrayBuffer

class NoRecodingGeocoderTest extends BaseTest {
  import TestValues._

  object BaseGeocoder extends Geocoder {
    private val geocodedBuff = new ArrayBuffer[Address]

    def geocoded = geocodedBuff.toSeq

    override def batchSize: Int = mockGeocoder.batchSize

    override def geocode(addresses: Seq[Address]): Seq[Option[LatLon]] = {
      // post condition: geocoded should return the sequence address geocoded (in order)
      geocodedBuff.clear()
      addresses.map { addr => geocodedBuff.append(addr); expected.get(addr)}
    }
  }

  // note: this unit tests that locations with coordinates are not regeocded
  // by the base geocoder when the geocodeLocations(.) function is used
  val base = BaseGeocoder
  val instance = new NoRecodingGeocoder(base)

  test("Should be able to geocode an empty sequence") {
    instance.geocodeLocations(Seq[Location]()).length should be (0)
    base.geocoded.length should be (0)
  }

  test("Should be able to skip regeocoding a location that already has coordinates") {
    val locationsNeedsRecoding = Seq(Location(addr0, None), loc1, loc2, Location(addr3, None))
    instance.geocodeLocations(locationsNeedsRecoding) should be (locations)
    base.geocoded should be (Seq(addr0, addr2, addr3))
  }
}
