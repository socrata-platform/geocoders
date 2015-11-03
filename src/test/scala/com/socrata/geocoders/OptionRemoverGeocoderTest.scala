package com.socrata.geocoders

class OptionRemoverGeocoderTest extends BaseTest {
  import TestValues._

  val instance = new OptionRemoverGeocoder(mockGeocoder)

  test("Should be able to geocode an empty sequence") {
    instance.geocode(Seq[Option[Address]]()).length should be (0)
    instance.geocodeLocations(Seq[Option[Location]]()).length should be (0)
  }

  test("Should geocode Nones to Nones") {
    val nones = Seq(None, None, None, None)
    instance.geocode(nones) should be (nones)
    instance.geocodeLocations(nones) should be (nones)
  }

  test("Should be able to geocode a sequence of all Somes") {
    instance.geocode(someAddresses) should be (coordinates)
    instance.geocodeLocations(someLocations) should be (someLocations)
  }

  test("Should be able to geocode maintaining correct order") {
    instance.geocode(addressesWithNones) should be (coordinatesWithNones)
    instance.geocodeLocations(locationsWithNones) should be (locationsWithNones)
  }
}
