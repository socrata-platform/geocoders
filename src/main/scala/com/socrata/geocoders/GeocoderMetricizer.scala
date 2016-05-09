package com.socrata.geocoders

class GeocoderMetricizer(counter: Long => Unit, underlying: Geocoder) extends Geocoder {
  override def batchSize: Int = underlying.batchSize

  override def geocode(addresses: Seq[InternationalAddress]): Seq[Option[LatLon]] = {
    val result = underlying.geocode(addresses)
    counter(result.length)
    result
  }
}
