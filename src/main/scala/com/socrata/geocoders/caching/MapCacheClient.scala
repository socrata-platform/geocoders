package com.socrata.geocoders.caching

import com.rojoma.json.v3.ast.JValue
import com.socrata.geocoders.{InternationalAddress, LatLon}

class MapCacheClient extends CacheClient {
  protected val map = scala.collection.mutable.Map[(Source, InternationalAddress), Option[LatLon]]()

  def cached = map.size

  def clear() = map.clear()

  override def cache(source: Source, addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit = {
    addresses.map { case (addr, (coord, info)) => map.put((source, addr), coord) }
  }

  override def lookup(source: Source, addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]] = {
    addresses.map { addr => map.get((source, addr)) }
  }
}
