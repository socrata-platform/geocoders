package com.socrata.geocoders.config

import com.typesafe.config.Config

import com.socrata.geocoders.caching.CassandraCacheClient

@deprecated(message="Use just 'CacheConfig'", since="2.4.0")
class CassandraCacheConfig(config: Config, root: String) extends CacheConfig(config, root) {
  val columnFamily = CassandraCacheClient.columnFamily
}
