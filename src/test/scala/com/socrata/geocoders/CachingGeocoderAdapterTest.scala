package com.socrata.geocoders

import com.socrata.geocoders.caching.CacheClient

class CachingGeocoderAdapterTest extends BaseTest {
  import TestValues._

  object MockMetric {
    private var cacheCount: Long = 0
    def count: Long = cacheCount
    def increment(count: Long): Unit = cacheCount += count
    def reset(): Unit = cacheCount = 0
  }

  object MockCache extends CacheClient {
    val cacheMap = scala.collection.mutable.Map[Address, Option[LatLon]]()

    def cached: Int = cacheMap.size

    def reset(): Unit = {
      cacheMap.clear()
    }

    override def lookup(addresses: Seq[Address]): Seq[Option[Option[LatLon]]] = addresses.map {
      addr => cacheMap.get(addr)
    }

    override def cache(addresses: Seq[(Address, Option[LatLon])]): Unit = addresses.map {
      case (addr, coord) => cacheMap.put(addr, coord)
    }
  }

  val metric = MockMetric
  val cache = MockCache
  val instance = new CachingGeocoderAdapter(cache, mockGeocoder, metric.increment)

  def reset(): Unit = {
    metric.reset()
    cache.reset()
  }

  test("Should be able to geocode an empty sequence") {
    reset()
    instance.geocode(Seq[Address]()).size should be (0)
    metric.count should be (0)

    reset()
    val toCache = addresses.zip(coordinates)
    cache.cache(toCache)
    instance.geocode(Seq[Address]()).size should be (0)
    metric.count should be (0)
    cache.cached should be (toCache.length)
  }

  test("Should cache all geocoding results when the cache is empty") {
    reset()
    instance.geocode(addresses) should be (coordinates)
    metric.count should be (0)
    cache.cached should be (addresses.length)
  }

  test("Should cache all geocoding results when the cache does not contain any of the values") {
    reset()
    val toCache = Seq((Address(Some("not_in_sequence"), None, None, None, None), None))
    cache.cache(toCache)
    instance.geocode(addresses) should be (coordinates)
    metric.count should be (0)
    cache.cached should be (addresses.length + 1)
  }

  test("Should use values found in the cache instead of regeocoding") {
    reset()
    val toCache = Seq((addr1, sll1), (addr2, sll2))
    cache.cache(toCache)
    instance.geocode(addresses) should be (coordinates)
    metric.count should be (2)
    cache.cached should be (addresses.length)

    reset()
    val modifiedCoordinates = coordinates.map {
      case Some(LatLon(lat, lon)) => Some(LatLon(lat + 1.0, lon - 1.0))
      case None => Some(LatLon(0.0, 0.0))
    }
    val toCache2 = addresses.zip(modifiedCoordinates)
    cache.cache(toCache2)
    instance.geocode(addresses) should be (modifiedCoordinates) // cache values are different then base geocoding
    metric.count should be (modifiedCoordinates.length)
    cache.cached should be (modifiedCoordinates.length)
  }

  test("Should count duplicate values as cached values") {
    reset()
    instance.geocode(addresses ++ Seq(addr1, addr1)) should be (coordinates ++ Seq(sll1, sll1))
    metric.count should be (2) // both duplicates of addr1
    cache.cached should be (addresses.length)

    reset()
    cache.cache(Seq((addr3, sll3)))
    instance.geocode(addresses ++ Seq(addr3, addr3)) should be (coordinates ++ Seq(sll3, sll3))
    metric.count should be (3) // addr3 in cache and both duplicate values
    cache.cached should be (addresses.length)

    reset()
    cache.cache(Seq((addr2, sll2)))
    instance.geocode(addresses ++ Seq(addr3, addr0)) should be (coordinates ++ Seq(sll3, sll0))
    metric.count should be (3) // addr2 in the cache, duplicates of addr0, addr3
    cache.cached should be (addresses.length)
  }
}
