package com.socrata.geocoders

class NoopOptionalGeocoderTest extends BaseTest {
  import TestValues._

  val instance = NoopOptionalGeocoder

  test("Should be able to geocode an empty sequence") {
    instance.geocode(Seq[Option[Address]]()).length should be (0)
    instance.geocodeLocations(Seq[Option[Location]]()).length should be (0)
  }

  test("Should geocode a sequence of addresses to a sequence of Nones of the same size") {
    instance.geocode(addressesWithNones) should be (addressesWithNones.map(_ => None))
  }

  test("Should geocode a sequence of locations to the same sequence of locations") {
    instance.geocodeLocations(locationsWithNones) should be (locationsWithNones)
  }
}
