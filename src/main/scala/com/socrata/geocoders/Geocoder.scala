package com.socrata.geocoders

import com.rojoma.json.v3.util.AutomaticJsonCodecBuilder

trait Geocoder {
  def batchSize: Int

  /**
   * @param addresses Addresses to geocode
   * @return The addresses, in the same order, geocoded.
   */
  def geocode(addresses: Seq[Address]): Seq[Option[LatLon]]

  /**
   * @param locations Locations to geocode
   * @return The locations, in the same order, geocoded.
   */
  def geocodeLocations(locations: Seq[Location]): Seq[Location] = {
    val addresses = locations.map(_.address)
    (addresses, geocode(addresses)).zipped.map(Location)
  }
}

trait OptionalGeocoder {
  def batchSize: Int

  /**
   * @param addresses Addresses to geocode
   * @return The addresses, in the same order, geocoded.
   */
  def geocode(addresses: Seq[Option[Address]]): Seq[Option[LatLon]]

  /**
   * @param locations Locations to geocode
   * @return The locations, in the same order, geocoded.
   */
  def geocodeLocations(locations: Seq[Option[Location]]): Seq[Option[Location]] = {
    val addresses = locations.map(_.map(_.address))
    (addresses, geocode(addresses)).zipped.map { (addressesOpt, latLonOpt) =>
      (addressesOpt, latLonOpt) match {
        case (Some(address), coordinates) => Some(Location(address, coordinates))
        case (None, None) => None
        case (None, _) => sys.error("Impossible: received a location for a nonexistent address")
      }
    }
  }
}

case class Address(address: Option[String], city: Option[String], state: Option[String], zip: Option[String], country: String) {
  // this deliberately excludes "country" because we _always_ have a country (we default to US)
  def isDefined: Boolean = address.isDefined || city.isDefined || state.isDefined || zip.isDefined
}

object Address {
  // filter out empty strings to None and default country to US
  def apply(address: Option[String], city: Option[String], state: Option[String], zip: Option[String], country: Option[String]): Address = {
    Address(address.filter(_.nonEmpty), city.filter(_.nonEmpty), state.filter(_.nonEmpty), zip.filter(_.nonEmpty), country.filter(_.nonEmpty).getOrElse("US")) //TODO: make default country configurable
  }
}

case class LatLon(lat: Double, lon: Double)

object LatLon {
  implicit val llCodec = AutomaticJsonCodecBuilder[LatLon]
}

case class Location(address: Address, coordinates: Option[LatLon])
