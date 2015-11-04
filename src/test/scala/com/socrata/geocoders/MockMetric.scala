package com.socrata.geocoders

class MockMetric {
  private var cached: Long = 0
  def count: Long = cached
  def increment(count: Long): Unit = cached += count
}
