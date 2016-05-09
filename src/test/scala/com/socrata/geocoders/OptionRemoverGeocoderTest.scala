package com.socrata.geocoders

class OptionRemoverGeocoderTest extends BaseTest {
  import TestValues._

  val instance = new OptionRemoverGeocoder(mockGeocoder)

  test("Should be able to geocode an empty sequence") {
    instance.geocode(Seq[Option[InternationalAddress]]()).length should be (0)
  }

  test("Should geocode Nones to Nones") {
    val nones = Seq(None, None, None, None)
    instance.geocode(nones) should be (nones)
  }

  test("Should be able to geocode a sequence of all Somes") {
    instance.geocode(someAddresses) should be (coordinates)
  }

  test("Should be able to geocode maintaining correct order") {
    instance.geocode(addressesWithNones) should be (coordinatesWithNones)
  }
}
