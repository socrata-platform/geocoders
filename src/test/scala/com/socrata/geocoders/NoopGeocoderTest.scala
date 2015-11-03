package com.socrata.geocoders

class NoopGeocoderTest extends BaseTest {
  import TestValues._

  val instance = NoopGeocoder

  test("Should be able to geocode an empty sequence") {
    instance.geocode(Seq[Address]()).length should be (0)
    instance.geocodeLocations(Seq[Location]()).length should be (0)
  }

  test("Should geocode a sequence of addresses to a sequence of Nones of the same size") {
    instance.geocode(addresses) should be (addresses.map(_ => None))
  }

  test("Should geocode a sequence of locations to the same sequence of locations") {
    instance.geocodeLocations(locations) should be (locations)
  }
}
