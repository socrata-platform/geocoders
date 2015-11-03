package com.socrata.geocoders

class GeocoderTest extends BaseTest {
  import TestValues._

  // note: this unit test is to test the geocodeLocations(.)
  // function as implemented in the Trait Geocoder
  val instance = mockGeocoder

  test("Should be able to geocode an empty sequence of locations") {
    instance.geocodeLocations(Seq[Location]()).length should be (0)
  }
  test("Should be able to maintain order while geocoding locations") {
    instance.geocodeLocations(locations) should be (locations)
  }
}
