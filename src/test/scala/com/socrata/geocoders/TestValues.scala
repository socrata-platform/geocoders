package com.socrata.geocoders

import com.rojoma.json.v3.ast.{JNull, JValue}

object TestValues {

  val addr0 = InternationalAddress(Some("0000 N 1st street"), None, None, None, None, "US")
  val addr1 = InternationalAddress(Some("1111 N 1st street"), None, None, None, None, "US")
  val addr2 = InternationalAddress(Some("No such street..."), None, None, None, None, "US")
  val addr3 = InternationalAddress(Some("3333 N 1st street"), None, None, None, None, "US")

  val saddr0 = Some(addr0)
  val saddr1 = Some(addr1)
  val saddr2 = Some(addr2)
  val saddr3 = Some(addr3)

  val ll0 = LatLon(0.0, 0.0)
  val ll1 = LatLon(1.0, 0.0)
  val ll3 = LatLon(3.0, 0.0)

  val sll0 = Some(ll0)
  val sll1 = Some(ll1)
  val sll2 = None
  val sll3 = Some(ll3)

  val expected = Map(
    addr0 -> ll0,
    addr1 -> ll1,
    addr3 -> ll3
  )

  val addresses   = Seq(addr0, addr1, addr2, addr3)
  val coordinates = Seq( sll0,  sll1,  sll2,  sll3)

  val someAddresses = addresses.map(Some(_))

  val addressesWithNones   = Seq(saddr0, None, saddr1, saddr2, None, saddr3, None)
  val coordinatesWithNones = Seq(  sll0, None,   sll1,   sll2, None,   sll3, None)

  val mockBaseGeocoder = new BaseGeocoder with Sourcable {
    override def batchSize: Int = 1

    override def geocode(addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] = {
      addresses.map { addr => (expected.get(addr), JNull) }
    }

    val source = caching.Source("mock")
  }

  val mockGeocoder = new Geocoder {
    override def batchSize: Int = 1

    override def geocode(addresses: Seq[InternationalAddress]): Seq[Option[LatLon]] = {
      addresses.map { addr => expected.get(addr) }
    }
  }

  val mockOptionalGeocoder = new OptionalGeocoder {
    override def batchSize: Int = 1

    override def geocode(addresses: Seq[Option[InternationalAddress]]): Seq[Option[LatLon]] = {
      addresses.map {{
        case Some(addr) => expected.get(addr)
        case None => None
      }}
    }
  }
}
