package com.socrata.geocoders

import com.rojoma.json.v3.ast.JValue

import scala.collection.mutable.ArrayBuffer

class CountryBatchingGeocoderAdapter(underlying: BaseGeocoder) extends BaseGeocoder {
  override def batchSize: Int = underlying.batchSize // this is not actually used

  /**
   * @param addresses Addresses to geocode
   * @return The addresses, in the same order, geocoded.
   */
  override def geocode(addresses: Seq[InternationalAddress]): Seq[(Option[LatLon], JValue)] = {
    if (addresses.isEmpty) return Seq.empty
    if (addresses.forall(_ == addresses.head)) return underlying.geocode(addresses) // don't do extra work

    // collect into batches by country value
    val batches = scala.collection.mutable.Map[String, ArrayBuffer[(InternationalAddress, Int)]]()
    for ((address, index) <- addresses.zipWithIndex) {
      val buf = batches.getOrElse(address.country, ArrayBuffer.empty)
      buf += ((address, index))
      batches += address.country -> buf
    }

    // geocode them separately
    val batched = batches.mapValues { batch =>
      val (toGeocode, indexes) = batch.toSeq.unzip
      val results = underlying.geocode(toGeocode)
      results.zip(indexes)
    }

    // put back into original order
    val geocoded = new Array[(Option[LatLon], JValue)](addresses.length)
    for ((result, index) <- batched.values.flatten) {
      geocoded(index) = result
    }
    geocoded
  }
}
