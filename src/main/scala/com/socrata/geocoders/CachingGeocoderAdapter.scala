package com.socrata.geocoders

import com.rojoma.json.v3.ast.JValue
import com.socrata.geocoders.caching.CacheClient

import scala.collection.mutable.ArrayBuffer


class CachingGeocoderAdapter(cacheClient: CacheClient, underlying: BaseGeocoder, cachedCounter: Long => Unit, multiplier: Int = 1) extends Geocoder {
  val log = org.slf4j.LoggerFactory.getLogger(classOf[CachingGeocoderAdapter])

  override def batchSize: Int = underlying.batchSize * multiplier

  override def geocode(addresses: Seq[InternationalAddress]): Seq[Option[LatLon]] = {
    if (addresses.isEmpty) return Seq.empty // don't go out to the cache here

    // ok.  This is somewhat subtle.  The problem: we want to geocode the least possible.
    // So: first let's de-dup the addresses, remembering where in "addresses" they
    // came from.  "deduped" is a map from the unique InternationalAddress objects to their indexes.
    val deduped = addresses.iterator.zipWithIndex.foldLeft(Map.empty[InternationalAddress, List[Int]]) { (acc, addrAndIndex) =>
      val (addr, idx) = addrAndIndex
      val idxes = idx :: acc.getOrElse(addr, Nil)
      acc + (addr -> idxes)
    }

    if(deduped.size != addresses.size) cachedCounter(addresses.size - deduped.size)

    // This just imposes _some_ order on the deduped addresses
    val orderedDeduped = deduped.keys.toIndexedSeq

    // This will hold the geocoded addresses, when all is done.
    // It is important that dedupedResult(i) == geocode(orderedDeduped(i))
    // because this is how we'll populate the result.
    val dedupedResult = new Array[Option[LatLon]](deduped.size)

    // We may already have some of the addresses cached.
    // "cached" will be the same length as "orderedDeduped" with
    // Some(Option[LatLon]) where something was cached and None where it was not.
    val cached = cacheClient.lookup(orderedDeduped)
    assert(cached.length == orderedDeduped.length)
    val cachedCount = cached.count(_.isDefined)
    if(cachedCount != 0) {
      log.info("Avoided re-geocoding {} addresses via the cache", cachedCount)
      cachedCounter(cachedCount)
    }

    // These are the ones that were not cached. We'll actually have to geocode those.
    val uncached = cached.zip(orderedDeduped).collect { case (None, ungeocoded) => ungeocoded }

    // Called to fill "dedupedResult" after geocoding actually happens.
    def reconstructResult(fresh: Seq[(Option[LatLon], JValue)]) {
      assert(fresh.length == uncached.length) // there must be one result for each hole in "cached"

      // Ok, stick the results of geocoding in the cache.
      cacheClient.cache(uncached.zip(fresh))

      // we'll be simultaneously scanning across "cached", "fresh",
      // and "dedupedResult" at different rates.
      val cacheIt = cached.iterator
      val freshIt = fresh.iterator
      var dst = 0

      def setResult(coordinate: Option[LatLon]) {
        dedupedResult(dst) = coordinate
        dst += 1
      }

      cacheIt.foreach {
        case Some(cachedCoordinate) => setResult(cachedCoordinate)
        case None  => setResult(freshIt.next()._1)
      }
      assert(freshIt.isEmpty)
    }

    if(uncached.isEmpty) reconstructResult(Seq[(Option[LatLon], JValue)]()) // Nothing to do; don't even bother going down a level
    else reconstructResult(underlying.geocode(uncached).toIndexedSeq)

    assert(dedupedResult.length == orderedDeduped.length)

    // Ok, now dedupedResult contains all the geocoded addresses in a 1:1 correspondence
    // with the original orderedDeduped.  We'll just scan across them/ and poke them into
    // the right places in the final result.
    val reduped = new Array[Option[LatLon]](addresses.size)
    (orderedDeduped, dedupedResult).zipped.foreach { (address, coordinates) =>
      deduped(address).foreach { idx => reduped(idx) = coordinates }
    }

    reduped
  }
}
