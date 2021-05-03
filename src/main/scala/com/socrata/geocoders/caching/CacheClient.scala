package com.socrata.geocoders.caching

import com.rojoma.json.v3.ast.JValue
import com.rojoma.json.v3.util.JsonUtil
import com.socrata.geocoders.{InternationalAddress, LatLon}

trait CacheClient {
  // creates a simple row identifier for an InternationalAddress
  // we are purposefully not doing anything smart here
  protected def toRowIdentifier(address: InternationalAddress): String =
    JsonUtil.renderJson(address, pretty = false)

  /**
   * @param addresses Sequence of InternationalAddresses to put in the cache
   */
  def cache(source: Source, addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit

  /**
   * @param addresses Sequence of InternationalAddresses to lookup from the cache
   * @return Sequence of results in a 1-1 correspondence to the addresses:
   *
   *         - Some(LatLon) if the address was found in the cache with a value
   *         - Some(None) if the address was found in the cache without a value
   *         - None if the address was not found in the cache
   */
  def lookup(source: Source, addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]]
}
