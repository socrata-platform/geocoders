package com.socrata.geocoders

import com.rojoma.json.v3.ast.JNull
import com.socrata.geocoders.caching.MapCacheClient

class CachingGeocoderAdapterTest extends BaseTest {
  import TestValues._

  def withMocks[A](f: (MapCacheClient, MockMetric, Geocoder) => A) = {
    val metric = new MockMetric
    val cache = new MapCacheClient
    val geocoder = new CachingGeocoderAdapter(cache, mockBaseGeocoder, metric.increment)
    f(cache, metric, geocoder)
  }

  test("Should be able to geocode an empty sequence") {
    withMocks { (_, metric, instance) =>
      instance.geocode(Seq[InternationalAddress]()).size should be (0)
      metric.count should be (0)
    }

    withMocks { (cache, metric, instance) =>
      val toCache = addresses.zip(coordinates.map { coord => (coord, JNull) })
      cache.cache(toCache)
      instance.geocode(Seq[InternationalAddress]()).size should be (0)
      metric.count should be (0)
      cache.cached should be (toCache.length)
    }
  }

  test("Should cache all geocoding results when the cache is empty") {
    withMocks { (cache, metric, instance) =>
      instance.geocode(addresses) should be(coordinates)
      metric.count should be(0)
      cache.cached should be(addresses.length)
    }
  }

  test("Should cache all geocoding results when the cache does not contain any of the values") {
    withMocks { (cache, metric, instance) =>
      val toCache = Seq((InternationalAddress(Some("not_in_sequence"), None, None, None, None, "US"), (None, JNull)))
      cache.cache(toCache)
      instance.geocode(addresses) should be(coordinates)
      metric.count should be(0)
      cache.cached should be(addresses.length + 1)
    }
  }

  test("Should use values found in the cache instead of regeocoding") {
    withMocks { (cache, metric, instance) =>
      val toCache = Seq((addr1, (sll1, JNull)), (addr2, (sll2, JNull)))
      cache.cache(toCache)
      instance.geocode(addresses) should be(coordinates)
      metric.count should be(2)
      cache.cached should be(addresses.length)
    }

    withMocks { (cache, metric, instance) =>
      val modifiedCoordinates = coordinates.map {
        case Some(LatLon(lat, lon)) => Some(LatLon(lat + 1.0, lon - 1.0))
        case None => Some(LatLon(0.0, 0.0))
      }
      val toCache = addresses.zip(modifiedCoordinates.map { coord => (coord, JNull) })
      cache.cache(toCache)
      instance.geocode(addresses) should be(modifiedCoordinates) // cache values are different then base geocoding
      metric.count should be(modifiedCoordinates.length)
      cache.cached should be(modifiedCoordinates.length)
    }
  }

  test("Should count duplicate values as cached values") {
    withMocks { (cache, metric, instance) =>
      instance.geocode(addresses ++ Seq(addr1, addr1)) should be(coordinates ++ Seq(sll1, sll1))
      metric.count should be(2) // both duplicates of addr1
      cache.cached should be(addresses.length)
    }

    withMocks { (cache, metric, instance) =>
      cache.cache(Seq((addr3, (sll3, JNull))))
      instance.geocode(addresses ++ Seq(addr3, addr3)) should be(coordinates ++ Seq(sll3, sll3))
      metric.count should be(3) // addr3 in cache and both duplicate values
      cache.cached should be(addresses.length)
    }

    withMocks { (cache, metric, instance) =>
      cache.cache(Seq((addr2, (sll2, JNull))))
      instance.geocode(addresses ++ Seq(addr3, addr0)) should be(coordinates ++ Seq(sll3, sll0))
      metric.count should be(3) // addr2 in the cache, duplicates of addr0, addr3
      cache.cached should be(addresses.length)
    }
  }
}
