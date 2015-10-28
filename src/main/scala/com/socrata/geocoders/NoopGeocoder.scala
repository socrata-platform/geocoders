package com.socrata.geocoders

object NoopGeocoder extends Geocoder {
  override def batchSize: Int = 100
  override def geocode(address: Seq[Address]): Seq[Option[LatLon]] = address.map(_ => None)
}

object NoopOptionalGeocoder extends OptionalGeocoder {
  override def batchSize: Int = 100
  override def geocode(address: Seq[Option[Address]]): Seq[Option[LatLon]] = address.map(_ => None)
}
