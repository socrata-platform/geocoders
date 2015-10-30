package com.socrata.geocoders.config

import com.socrata.thirdparty.typesafeconfig.{ConfigClass, CassandraConfig}
import com.typesafe.config.Config

class GeocodingConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val filterMultipier = getInt("filter-multiplier")
  val cache = optionally(getRawConfig("cache")) map { _ =>
    getConfig("cache", new GeocodingCacheConfig(_, _))
  }

  val mapQuest = optionally(getRawConfig("mapquest")) map { _ =>
    getConfig("mapquest", new MapQuestGeocodingConfig(_, _))
  }
}

class MapQuestGeocodingConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val appToken = getString("app-token")
  val retryCount = getInt("retry-count")
}

class GeocodingCacheConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val cassandraClient = optionally(getRawConfig("cassandra-client")) map { _ =>
    getConfig("cassandra-client", new CassandraCacheConfig(_, _))
  }
}

class CassandraCacheConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val columnFamily = getString("column-family")
  val ttl = getDuration("ttl")
  val cassandra = getConfig("cassandra", new CassandraConfig(_, _))
}
