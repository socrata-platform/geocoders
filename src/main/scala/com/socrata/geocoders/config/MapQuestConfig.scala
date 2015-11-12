package com.socrata.geocoders.config

import com.socrata.thirdparty.typesafeconfig.ConfigClass
import com.typesafe.config.Config

// Config class for use with MapQuestGeocoder
class MapQuestConfig(config: Config, root: String) extends ConfigClass(config, root) {
  val appToken = getString("app-token")
  val retryCount = getInt("retry-count")
}
