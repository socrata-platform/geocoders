package com.socrata.geocoders

import com.rojoma.json.v3.ast.JNull

class CountryBatchingGeocoderAdapterTest extends BaseTest {
  import TestValues._

  val instance = new CountryBatchingGeocoderAdapter(singleCountryMockBaseGeocoder)

  test("Should be able to geocode an empty sequence") {
    instance.geocode(Seq.empty).length should be (0)
  }

  test("Should be able to geocode a sequence of addresses from the same country") {
    instance.geocode(Seq(addr0, addr1, addr2, addr3)) should be(Seq(sll0, sll1, sll2, sll3).map { sll => (sll, JNull) })
  }

  test("Should be able to geocode a sequence of addresses from different countries") {
    val addresses = Seq(addr0, addr4, addr1, addr2, addr6, addr5, addr7, addr3)
    val latLons = addresses.map { addr => (expected.get(addr), JNull) }
    instance.geocode(addresses) should be(latLons)
  }

}
