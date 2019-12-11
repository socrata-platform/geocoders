package com.socrata.geocoders.caching

import com.datastax.oss.driver.api.core.CqlSession
import com.datastax.oss.driver.api.core.cql.{BatchStatement, PreparedStatement, AsyncResultSet}
import com.datastax.oss.driver.api.core.`type`.codec.{TypeCodec, TypeCodecs}
import com.rojoma.json.v3.io.{CompactJsonWriter, JsonReader}
import com.rojoma.json.v3.matcher.{FirstOf, Variable, PArray}
import com.rojoma.json.v3.ast.{JNull, JValue}
import com.socrata.geocoders.{InternationalAddress, LatLon}
import java.util.concurrent.{Semaphore, CompletionStage}
import java.util.function.BiConsumer

import scala.concurrent.duration.FiniteDuration

class CassandraCacheClient(keyspace: CqlSession,
                           columnFamily: String,
                           cacheTime: FiniteDuration,
                           concurrencyLimit: Int = 1000) extends CacheClient {
  val cacheTTL = cacheTime.toSeconds.toInt

  val column = "coords"

  val latLon = Variable[LatLon]()
  val annotation = Variable[JValue]()
  val Pattern = PArray(
    FirstOf(latLon, JNull),
    annotation
  )

  val log = org.slf4j.LoggerFactory.getLogger(classOf[CassandraCacheClient])

  val lookupQuery = keyspace.prepare("select coords from " + columnFamily + " where address = ?")

  private val tickets = new Semaphore(concurrencyLimit)

  private val freeTicket = new BiConsumer[Any, Any] {
    def accept(a: Any, b: Any) {
      tickets.release()
    }
  }

  override def lookup(addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]] = {
    // postcondition: result.length == addresses.length
    addresses.map { address =>
      val boundQuery = lookupQuery.bind(toRowIdentifier(address))
      tickets.acquire()
      try {
        keyspace.
          executeAsync(boundQuery).
          whenComplete(freeTicket)
      } catch {
        case t: Throwable =>
          // either the executaAsync or the whenComplete threw, so
          // release the ticket to prevent it from getting lost
          // permanently
          tickets.release()
          throw t
      }
    }.map { rsFuture : CompletionStage[AsyncResultSet] =>
      Option(rsFuture.toCompletableFuture.get().one).map { elem =>
        elem.get(0, TypeCodecs.TEXT)
      }
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

  val cacheStmt = keyspace.prepare("insert into " + columnFamily + " (address, coords) values (?, ?) using ttl " + cacheTTL)
  override def cache(addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit = {
    addresses.map { case (address, (point, ann)) =>
      val boundQuery = cacheStmt.bind(toRowIdentifier(address), CompactJsonWriter.toString(Pattern.generate(latLon :=? point, annotation := ann)))
      tickets.acquire()
      try {
        keyspace.
          executeAsync(boundQuery).
          whenComplete(freeTicket)
      } catch {
        case t: Throwable =>
          tickets.release()
          throw t
      }
    }.foreach(_.toCompletableFuture.get())
  }
}
