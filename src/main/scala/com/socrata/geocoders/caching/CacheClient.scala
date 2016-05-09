package com.socrata.geocoders.caching

import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.codec.JsonEncode
import com.rojoma.json.v3.io.CompactJsonWriter
import com.socrata.geocoders.{InternationalAddress, LatLon}

import scala.collection.immutable.SortedMap

trait CacheClient {
  // this encoder produces a _canonical_ encoding of the
  // address -- in particular, the fields are sorted.
  val addressEncoder = new JsonEncode[InternationalAddress] {
    override def encode(addr: InternationalAddress): JValue = {
      val InternationalAddress(address, locality, subregion, region, postalCode, country) = addr
      val intermediate = SortedMap.newBuilder[String, String]
      address.foreach(intermediate += "address" -> _)
      locality.foreach(intermediate += "locality" -> _)
      region.foreach(intermediate += "subregion" -> _)
      region.foreach(intermediate += "region" -> _)
      postalCode.foreach(intermediate += "postal_code" -> _)
      intermediate += "country" -> country
      JsonEncode.toJValue(intermediate.result())
    }
  }

  // creates a simple row identifier for an InternationalAddress
  // we are purposefully not doing anything smart here
  protected def toRowIdentifier(address: InternationalAddress): String = {
    CompactJsonWriter.toString(addressEncoder.encode(address))
  }

  /**
   * @param addresses Sequence of InternationalAddresses to put in the cache
   */
  def cache(addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit

  /**
   * @param addresses Sequence of InternationalAddresses to lookup from the cache
   * @return Sequence of results in a 1-1 correspondence to the addresses:
   *
   *         - Some(LatLon) if the address was found in the cache with a value
   *         - Some(None) if the address was found in the cache without a value
   *         - None if the address was not found in the cache
   */
  def lookup(addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]]
}
