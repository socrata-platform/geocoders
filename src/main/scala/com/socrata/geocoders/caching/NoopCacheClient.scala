package com.socrata.geocoders.caching

import com.socrata.geocoders.{Address, LatLon}

object NoopCacheClient extends CacheClient {
  override def cache(addresses: Seq[(Address, Option[LatLon])]): Unit = {
    // do nothing! :D
  }

  override def lookup(addresses: Seq[Address]): Seq[Option[Option[LatLon]]] = {
    addresses.map(_ => None)
  }
}
