package com.socrata.geocoders

class OptionalGeocoderTest extends BaseTest {
  import TestValues._

  // note: this unit test is to test the geocodeLocations(.)
  // function as implemented in the Trait OptionalGeocoder
  val instance = mockOptionalGeocoder

  test("Should be able to geocode an empty sequence of locations") {
    instance.geocodeLocations(Seq[Option[Location]]()).length should be (0)
  }

  test("Should geocode Nones to Nones for locations") {
    val nones = Seq(None, None, None, None)
    instance.geocodeLocations(nones) should be (nones)
  }

  test("Should be able to maintain the order of locations") {
    instance.geocodeLocations(someLocations) should be (someLocations)
  }

  test("Should be able to maintain the order of locations and Nones") {
    instance.geocodeLocations(locationsWithNones) should be (locationsWithNones)
  }
}
