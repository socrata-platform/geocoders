package com.socrata.geocoders.caching

import com.socrata.geocoders.{LatLon, Address}

class MapCacheClient extends CacheClient {
  protected val map = scala.collection.mutable.Map[Address, Option[LatLon]]()

  def cached = map.size

  def clear() = map.clear()

  override def cache(addresses: Seq[(Address, Option[LatLon])]): Unit = {
    addresses.map { case (addr, coord) => map.put(addr, coord) }
  }

  override def lookup(addresses: Seq[Address]): Seq[Option[Option[LatLon]]] = {
    addresses.map { addr => map.get(addr) }
  }
}
