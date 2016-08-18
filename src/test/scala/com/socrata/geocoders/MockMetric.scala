package com.socrata.geocoders

class MockMetric {
  private var cached: Long = 0
  def count: Long = cached
  def increment(count: Long): Unit = cached += count
}

class GeocodeMockMetric {
  private var geocoded: Long = 0
  def count: Long = geocoded
  def increment(result: GeocodingResult, count: Long): Unit = geocoded += count
}
