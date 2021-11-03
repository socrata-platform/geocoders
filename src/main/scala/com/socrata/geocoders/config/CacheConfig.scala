package com.socrata.geocoders.config

import com.socrata.thirdparty.typesafeconfig.ConfigClass
import com.typesafe.config.Config

// Config class for use with PostgresqlCacheClient
class CacheConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val preference: Option[CacheConfig.Preference] = optionally(getString("preference")) map {
    case "postgresql" => CacheConfig.Postgresql
    case "none" => CacheConfig.None
    case _ => throw new Exception("Invalid value for " + path("preference") + "; should be 'postgres' or 'none'")
  }
  val ttl = getDuration("ttl")
}

object CacheConfig {
  sealed trait Preference
  case object Postgresql extends Preference
  case object None extends Preference
}
