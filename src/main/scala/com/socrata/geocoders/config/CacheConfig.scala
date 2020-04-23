package com.socrata.geocoders.config

import com.socrata.thirdparty.typesafeconfig.ConfigClass
import com.typesafe.config.Config

// Config class for use with CassandraCacheClient
class CacheConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val preference: Option[CacheConfig.Preference] = optionally(getString("preference")) map {
    case "postgresql" => CacheConfig.Postgresql
    case "cassandra" => CacheConfig.Cassandra
    case "none" => CacheConfig.None
    case _ => throw new Exception("Invalid value for " + path("preference") + "; should be 'postgres', 'cassandra', or 'none'")
  }
  val ttl = getDuration("ttl")
}

object CacheConfig {
  sealed trait Preference
  case object Postgresql extends Preference
  case object Cassandra extends Preference
  case object None extends Preference
}
