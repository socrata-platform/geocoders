package com.socrata.geocoders.config

import com.rojoma.json.v3.ast.JString

import com.socrata.thirdparty.typesafeconfig.ConfigClass
import com.typesafe.config.Config

sealed trait GeocoderConfig extends ConfigClass

// Config class for use with MapQuestGeocoder
class MapQuestConfig(config: Config, root: String) extends ConfigClass(config, root) with GeocoderConfig {
  val appToken = getString("app-token")
  val retryCount = getInt("retry-count")
}

// Config class for use with EsriGeocoder
class EsriConfig(config: Config, root: String) extends ConfigClass(config, root) with GeocoderConfig {
  val appToken = getString("app-token")
  val host = getString("host")
  val retryCount = getInt("retry-count")
  val batchSizeRefresh = getDuration("batch-size-refresh")
}

object GeocoderConfig extends ((Config, String) => GeocoderConfig) {
  override def apply(config: Config, root: String): GeocoderConfig = {
    object Selector extends ConfigClass(config, root) {
      val typ = getString("type")
    }
    Selector.typ match {
      case "mapquest" => new MapQuestConfig(config, root)
      case "esri" => new EsriConfig(config, root)
      case other => throw new Exception("Unknown geocoder type " + JString(other))
    }
  }
}
