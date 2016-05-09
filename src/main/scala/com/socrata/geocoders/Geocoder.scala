package com.socrata.geocoders

import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.util.AutomaticJsonCodecBuilder


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

case class InternationalAddress(address: Option[String],
                                locality: Option[String],
                                subregion: Option[String],
                                region: Option[String],
                                postalCode: Option[String],
                                country: String)

object InternationalAddress {
  // filter out empty strings to None and default country to US
  def apply(address: Option[String],
            locality: Option[String],
            subregion: Option[String],
            region: Option[String],
            postalCode: Option[String],
            country: Option[String]): Option[InternationalAddress] = {
    if (Seq(address, locality, subregion, region, postalCode, country).forall(_.isEmpty)) None
    else Some(new InternationalAddress(
      address.filter(_.nonEmpty),
      locality.filter(_.nonEmpty),
      subregion.filter(_.nonEmpty),
      region.filter(_.nonEmpty),
      postalCode.filter(_.nonEmpty),
      country.filter(_.nonEmpty).getOrElse("US")))
  }
}

object USAddress {
  def apply(address: Option[String],
            city: Option[String],
            state: Option[String],
            zip: Option[String]): Option[InternationalAddress] = {
    InternationalAddress(address, city, None, state, zip, None)
  }
}

case class LatLon(lat: Double, lon: Double)

object LatLon {
  implicit val llCodec = AutomaticJsonCodecBuilder[LatLon]
}
