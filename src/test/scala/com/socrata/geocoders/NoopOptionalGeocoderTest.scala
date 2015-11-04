package com.socrata.geocoders

class NoopOptionalGeocoderTest extends BaseTest {
  import TestValues._

  val instance = NoopOptionalGeocoder

  test("Should be able to geocode an empty sequence") {
    instance.geocode(Seq[Option[Address]]()).length should be (0)
  }

  test("Should geocode a sequence of addresses to a sequence of Nones of the same size") {
    instance.geocode(addressesWithNones) should be (addressesWithNones.map(_ => None))
  }
}
