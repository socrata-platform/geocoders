package com.socrata.geocoders

import com.rojoma.json.v3.ast.JValue


trait BaseGeocoder {
  def batchSize: Int

  /**
   * @param addresses Addresses to geocode
   * @return The addresses, in the same order, geocoded with JValue annotation.
   */
  def geocode(addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)]
}

trait Geocoder {
  def batchSize: Int

  /**
   * @param addresses Addresses to geocode
   * @return The addresses, in the same order, geocoded.
   */
  def geocode(addresses: Seq[InternationalAddress]): Seq[Option[LatLon]]
}

trait OptionalGeocoder {
  def batchSize: Int

  /**
   * @param addresses Addresses to geocode
   * @return The addresses, in the same order, geocoded.
   */
  def geocode(addresses: Seq[Option[InternationalAddress]]): Seq[Option[LatLon]]
}
