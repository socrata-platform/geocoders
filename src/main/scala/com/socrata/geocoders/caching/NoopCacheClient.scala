package com.socrata.geocoders.caching

import com.rojoma.json.v3.ast.JValue
import com.socrata.geocoders.{InternationalAddress, LatLon}

object NoopCacheClient extends CacheClient {
  override def cache(source: Source, addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit = {
    // do nothing! :D
  }

  override def lookup(source: Source, addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]] = {
    addresses.map(_ => None)
  }
}
