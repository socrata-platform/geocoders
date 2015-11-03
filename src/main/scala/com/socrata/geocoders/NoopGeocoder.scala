package com.socrata.geocoders

object NoopGeocoder extends Geocoder {
  override def batchSize: Int = 100
  override def geocode(address: Seq[Address]): Seq[Option[LatLon]] = address.map(_ => None)
  override def geocodeLocations(locations: Seq[Location]): Seq[Location] = locations
}

object NoopOptionalGeocoder extends OptionalGeocoder {
  override def batchSize: Int = 100
  override def geocode(address: Seq[Option[Address]]): Seq[Option[LatLon]] = address.map(_ => None)
  override def geocodeLocations(locations: Seq[Option[Location]]): Seq[Option[Location]] = locations
}
