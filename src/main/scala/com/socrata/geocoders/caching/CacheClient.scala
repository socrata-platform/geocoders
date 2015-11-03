package com.socrata.geocoders.caching

import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.codec.JsonEncode
import com.rojoma.json.v3.io.CompactJsonWriter
import com.socrata.geocoders.{Address, LatLon}

import scala.collection.immutable.SortedMap

trait CacheClient {
  // this encoder produces a _canonical_ encoding of the
  // address -- in particular, the fields are sorted.
  val addressEncoder = new JsonEncode[Address] {
    override def encode(addr: Address): JValue = {
      val Address(address, city, state, zip, country) = addr
      val intermediate = SortedMap.newBuilder[String, String]
      address.foreach(intermediate += "address" -> _)
      city.foreach(intermediate += "city" -> _)
      state.foreach(intermediate += "state" -> _)
      zip.foreach(intermediate += "zip" -> _)
      intermediate += "country" -> country
      JsonEncode.toJValue(intermediate.result())
    }
  }

  // creates a simple row identifier for an Address
  // we are purposefully not doing anything smart here
  protected def toRowIdentifier(address: Address): String = {
    CompactJsonWriter.toString(addressEncoder.encode(address))
  }

  /**
   * @param addresses Sequence of Addresses to put in the cache
   */
  def cache(addresses: Seq[(Address, Option[LatLon])]): Unit

  /**
   * @param addresses Sequence of Addresses to lookup from the cache
   * @return Sequence of results in a 1-1 correspondence to the addresses:
   *
   *         - Some(LatLon) if the address was found in the cache with a value
   *         - Some(None) if the address was found in the cache without a value
   *         - None if the address was not found in the cache
   */
  def lookup(addresses: Seq[Address]): Seq[Option[Option[LatLon]]]
}
