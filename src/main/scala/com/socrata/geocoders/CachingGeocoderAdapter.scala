package com.socrata.geocoders

import com.socrata.geocoders.caching.CacheClient

import scala.collection.mutable.ArrayBuffer


class CachingGeocoderAdapter(cacheClient: CacheClient, underlying: Geocoder, cachedCounter: Long => Unit, multiplier: Int = 1) extends Geocoder {
  val log = org.slf4j.LoggerFactory.getLogger(classOf[CachingGeocoderAdapter])

  override def batchSize: Int = underlying.batchSize * multiplier

  override def geocode(addresses: Seq[Address]): Seq[Option[LatLon]] = {
    // ok.  This is somewhat subtle.  The problem: we want to geocode the least possible.
    // So: first let's de-dup the addresses, remembering where in "addresses" they
    // came from.  "deduped" is a map from the unique Address objects to their indexes.
    val deduped = addresses.iterator.zipWithIndex.foldLeft(Map.empty[Address, List[Int]]) { (acc, addrAndIndex) =>
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
    val dedupedResult = new ArrayBuffer[Option[LatLon]](deduped.size)

    // Not all addreses will need geocoding.  This holds only the addresses
    // that need geocoding, preserving order.
    val toGeocode = new ArrayBuffer[Address](deduped.size)

    // Only keep addresses that have some street, city, state, or zip.
    // Addresses that are not defined should get the result of None.
    // We'll store them in the right place in dedupedResults straight
    // away.
    for(address <- orderedDeduped) {
      if(!address.isDefined) { dedupedResult += None }
      else { dedupedResult += null; toGeocode += address }
    }
    assert(dedupedResult.length == orderedDeduped.length)

    // If all addresses are not defined (and we don't need to geocode), we can just return all Nones
    if(toGeocode.isEmpty) return addresses.map(_ => None)

    // We may already have some of the addresses cached.
    // "cached" will be the same length as "toGeocode" with
    // Some(Option[LatLon]) where something was cached and None where it was not.
    val cached = cacheClient.lookup(toGeocode)
    assert(cached.length == toGeocode.length)
    val cachedCount = cached.count(_.isDefined)
    if(cachedCount != 0) {
      log.info("Avoided re-geocoding {} addresses via the cache", cachedCount)
      cachedCounter(cachedCount)
    }

    // These are the ones that were not cached. We'll actually have to geocode those.
    val uncached = cached.zip(toGeocode).collect { case (None, ungeocoded) => ungeocoded }

    // Called to fill "dedupedResult" after geocoding actually happens.
    def reconstructResult(fresh: Seq[Option[LatLon]]) {
      assert(fresh.length == uncached.length) // there must be one result for each hole in "cached"

      // Ok, stick the results of geocoding in the cache.
      cacheClient.cache(uncached.zip(fresh))

      // we'll be simultaneously scanning across "cached", "fresh",
      // and "dedupedResult" at different rates.
      val cacheIt = cached.iterator
      val freshIt = fresh.iterator
      var dst = 0

      def setResult(coordinate: Option[LatLon]) {
        while(dedupedResult(dst) ne null) dst += 1
        dedupedResult(dst) = coordinate
      }

      cacheIt.foreach {
        case Some(cachedCoordinate) => setResult(cachedCoordinate)
        case None  => setResult(freshIt.next())
      }
      assert(freshIt.isEmpty)
    }

    if(uncached.isEmpty) reconstructResult(Seq[Option[LatLon]]()) // Nothing to do; don't even bother going down a level
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
