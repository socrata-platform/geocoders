package com.socrata.geocoders

import scala.collection.mutable.ArrayBuffer

class NoRecodingGeocoder(underlying: Geocoder, multiplier: Int = 1) extends Geocoder {
  override def batchSize: Int = underlying.batchSize * multiplier

  override def geocode(addresses: Seq[Address]): Seq[Option[LatLon]] = underlying.geocode(addresses)

  override def geocodeLocations(locations: Seq[Location]): Seq[Location] = {
    val result = new ArrayBuffer[Location](locations.length)
    val toGeocode = new ArrayBuffer[Location](locations.length)
    locations foreach { location =>
      location.coordinate match {
          case Some(latLon) => result += location
          case None => result += null; toGeocode += location
        }
    }

    if(toGeocode.isEmpty) result
    else {
      var i = 0
      for(geocoded <- underlying.geocodeLocations(toGeocode)) {
        while(result(i) ne null) i += 1
        result(i) = geocoded
      }
      result
    }
  }
}
