package com.socrata.geocoders

import com.rojoma.json.v3.ast.{JNull, JValue}

object BaseNoopGeocoder extends BaseGeocoder {
  override def batchSize: Int = 100 // value does not matter
  override def geocode(address: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] = address.map(_ => (None, JNull))
}

object NoopGeocoder extends Geocoder {
  override def batchSize: Int = 100 // value does not matter
  override def geocode(address: Seq[InternationalAddress]): Seq[Option[LatLon]] = address.map(_ => None)
}

object NoopOptionalGeocoder extends OptionalGeocoder {
  override def batchSize: Int = 100 // value does not matter
  override def geocode(address: Seq[Option[InternationalAddress]]): Seq[Option[LatLon]] = address.map(_ => None)
}
