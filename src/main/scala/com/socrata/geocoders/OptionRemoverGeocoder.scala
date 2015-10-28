package com.socrata.geocoders

import scala.collection.mutable.ArrayBuffer

class OptionRemoverGeocoder(underlying: Geocoder, multiplier: Int = 1) extends OptionalGeocoder {
  override def batchSize: Int = underlying.batchSize * multiplier

  override def geocode(addresses: Seq[Option[Address]]): Seq[Option[LatLon]] = {
    val result = new ArrayBuffer[Option[LatLon]](addresses.length)
    val toGeocode = new ArrayBuffer[Address](addresses.length)
    addresses foreach {
      case Some(address) => result += null; toGeocode += address
      case None => result += None
    }

    if(toGeocode.isEmpty) result
    else {
      var i = 0
      for(geocoded <- underlying.geocode(toGeocode)) {
        while(result(i) ne null) i += 1
        result(i) = geocoded
      }
      result
    }
  }
}
