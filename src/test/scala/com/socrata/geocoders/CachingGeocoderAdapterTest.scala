package com.socrata.geocoders

import com.socrata.geocoders.caching.MapCacheClient

class CachingGeocoderAdapterTest extends BaseTest {
  import TestValues._

  case class Instance(cache: MapCacheClient, metric: MockMetric, geocoder: Geocoder)

  def geocoder: Instance = withMocks[Instance] { case (cache, metric, geocoder) => Instance(cache, metric, geocoder) }

  def withMocks[A](f: (MapCacheClient, MockMetric, Geocoder) => A) = {
    val metric = new MockMetric
    val cache = new MapCacheClient
    val geocoder = new CachingGeocoderAdapter(cache, mockGeocoder, metric.increment)
    f(cache, metric, geocoder) }

  test("Should be able to geocode an empty sequence") {
    val Instance(_, metric, instance) = geocoder
    instance.geocode(Seq[Address]()).size should be (0)
    metric.count should be (0)

    val Instance(cache2, metric2, instance2) = geocoder
    val toCache = addresses.zip(coordinates)
    cache2.cache(toCache)
    instance2.geocode(Seq[Address]()).size should be (0)
    metric2.count should be (0)
    cache2.cached should be (toCache.length)
  }

  test("Should cache all geocoding results when the cache is empty") {
    val Instance(cache, metric, instance) = geocoder
    instance.geocode(addresses) should be (coordinates)
    metric.count should be (0)
    cache.cached should be (addresses.length)
  }

  test("Should cache all geocoding results when the cache does not contain any of the values") {
    val Instance(cache, metric, instance) = geocoder
    val toCache = Seq((Address(Some("not_in_sequence"), None, None, None, None), None))
    cache.cache(toCache)
    instance.geocode(addresses) should be (coordinates)
    metric.count should be (0)
    cache.cached should be (addresses.length + 1)
  }

  test("Should use values found in the cache instead of regeocoding") {
    val Instance(cache, metric, instance) = geocoder
    val toCache = Seq((addr1, sll1), (addr2, sll2))
    cache.cache(toCache)
    instance.geocode(addresses) should be (coordinates)
    metric.count should be (2)
    cache.cached should be (addresses.length)

    val Instance(cache2, metric2, instance2) = geocoder
    val modifiedCoordinates = coordinates.map {
      case Some(LatLon(lat, lon)) => Some(LatLon(lat + 1.0, lon - 1.0))
      case None => Some(LatLon(0.0, 0.0))
    }
    val toCache2 = addresses.zip(modifiedCoordinates)
    cache2.cache(toCache2)
    instance2.geocode(addresses) should be (modifiedCoordinates) // cache values are different then base geocoding
    metric2.count should be (modifiedCoordinates.length)
    cache2.cached should be (modifiedCoordinates.length)
  }

  test("Should count duplicate values as cached values") {
    val Instance(cache, metric, instance) = geocoder
    instance.geocode(addresses ++ Seq(addr1, addr1)) should be (coordinates ++ Seq(sll1, sll1))
    metric.count should be (2) // both duplicates of addr1
    cache.cached should be (addresses.length)

    val Instance(cache2, metric2, instance2) = geocoder
    cache2.cache(Seq((addr3, sll3)))
    instance2.geocode(addresses ++ Seq(addr3, addr3)) should be (coordinates ++ Seq(sll3, sll3))
    metric2.count should be (3) // addr3 in cache and both duplicate values
    cache2.cached should be (addresses.length)

    val Instance(cache3, metric3, instance3) = geocoder
    cache3.cache(Seq((addr2, sll2)))
    instance3.geocode(addresses ++ Seq(addr3, addr0)) should be (coordinates ++ Seq(sll3, sll0))
    metric3.count should be (3) // addr2 in the cache, duplicates of addr0, addr3
    cache3.cached should be (addresses.length)
  }
}
