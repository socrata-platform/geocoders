package com.socrata.geocoders

import com.rojoma.json.v3.util.JsonUtil
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import Arbitrary.arbitrary

class InternationalAddressTest extends BaseTest with PropertyChecks {
  val genAddress = for {
    address <- arbitrary[Option[String]]
    locality <- arbitrary[Option[String]]
    subregion <- arbitrary[Option[String]]
    region <- arbitrary[Option[String]]
    postalCode <- arbitrary[Option[String]]
    country <- arbitrary[String]
  } yield InternationalAddress(address, locality, subregion, region, postalCode, country)

  implicit val arbAddress = Arbitrary(genAddress)

  forAll { (addr: InternationalAddress) =>
    JsonUtil.parseJson[InternationalAddress](JsonUtil.renderJson(addr)) should equal (Right(addr))
  }
}
