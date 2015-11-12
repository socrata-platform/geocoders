package com.socrata.geocoders.config

import com.socrata.thirdparty.typesafeconfig.ConfigClass
import com.typesafe.config.Config

// Config class for use with CassandraCacheClient
class CassandraCacheConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val columnFamily = getString("column-family")
  val ttl = getDuration("ttl")
}
