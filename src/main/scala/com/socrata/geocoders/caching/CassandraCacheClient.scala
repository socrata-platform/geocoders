package com.socrata.geocoders.caching

import com.datastax.driver.core.{TypeCodec, BatchStatement, PreparedStatement, Session}
import com.rojoma.json.v3.io.{CompactJsonWriter, JsonReader}
import com.rojoma.json.v3.matcher.{FirstOf, Variable, PArray}
import com.rojoma.json.v3.ast.{JNull, JValue}
import com.socrata.geocoders.{InternationalAddress, LatLon}

import scala.concurrent.duration.FiniteDuration

class CassandraCacheClient(keyspace: Session, columnFamily: String, cacheTime: FiniteDuration, pqCache: (Session, String) => PreparedStatement = _.prepare(_)) extends CacheClient {
  val cacheTTL = cacheTime.toSeconds.toInt

  val column = "coords"

  val latLon = Variable[LatLon]()
  val annotation = Variable[JValue]()
  val Pattern = PArray(
    FirstOf(latLon, JNull),
    annotation
  )

  val lookupQuery = pqCache(keyspace, "select coords from " + columnFamily + " where address = ?")

  override def lookup(addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]] = {
    // postcondition: result.length == addresses.length
    addresses
      .map { address => keyspace.executeAsync(lookupQuery.bind(toRowIdentifier(address))) }
      .map { rsFuture =>
        val it = rsFuture.get().iterator
        if(it.hasNext) Some(it.next().get(0, TypeCodec.varchar))
        else None
      }.map { (result: Option[String]) =>
        result.map { jsonText =>
          JsonReader.fromString(jsonText) match {
            case Pattern(res) =>
              latLon.get(res)
            case _ =>
              None
          }
        }
      }
  }

  val cacheStmt = pqCache(keyspace, "insert into " + columnFamily + " (address, coords) values (?, ?) using ttl " + cacheTTL)
  override def cache(addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit = {
    if(addresses.nonEmpty) {
      val mutation = new BatchStatement
      for((address, (point, ann)) <- addresses) {
        mutation.add(cacheStmt.bind(toRowIdentifier(address), CompactJsonWriter.toString(Pattern.generate(latLon :=? point, annotation := ann))))
      }
      keyspace.execute(mutation)
    }
  }
}
