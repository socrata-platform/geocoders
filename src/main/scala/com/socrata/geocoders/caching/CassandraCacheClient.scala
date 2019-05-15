package com.socrata.geocoders.caching

import com.datastax.driver.core.{TypeCodec, BatchStatement, PreparedStatement, Session}
import com.google.common.util.concurrent.MoreExecutors
import com.rojoma.json.v3.io.{CompactJsonWriter, JsonReader}
import com.rojoma.json.v3.matcher.{FirstOf, Variable, PArray}
import com.rojoma.json.v3.ast.{JNull, JValue}
import com.socrata.geocoders.{InternationalAddress, LatLon}
import java.util.concurrent.Semaphore

import scala.concurrent.duration.FiniteDuration

class CassandraCacheClient(keyspace: Session,
                           columnFamily: String,
                           cacheTime: FiniteDuration,
                           pqCache: (Session, String) => PreparedStatement = _.prepare(_),
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

  val lookupQuery = pqCache(keyspace, "select coords from " + columnFamily + " where address = ?")

  private val tickets = new Semaphore(concurrencyLimit)
  private val freeTicket = new Runnable {
    def run() {
      tickets.release()
    }
  }

  override def lookup(addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]] = {
    // postcondition: result.length == addresses.length
    addresses.map { address =>
      val boundQuery = lookupQuery.bind(toRowIdentifier(address))
      tickets.acquire()
      try {
        val rsFuture = keyspace.executeAsync(boundQuery)
        rsFuture.addListener(freeTicket, MoreExecutors.directExecutor)
        rsFuture
      } catch {
        case t: Throwable =>
          // either the executaAsync or the addListener threw, so
          // release the ticket to prevent it from getting lost
          // permanently
          tickets.release()
          throw t
      }
    }.map { rsFuture =>
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
    addresses.map { case (address, (point, ann)) =>
      val boundQuery = cacheStmt.bind(toRowIdentifier(address), CompactJsonWriter.toString(Pattern.generate(latLon :=? point, annotation := ann)))
      tickets.acquire()
      try {
        val insFuture = keyspace.executeAsync(boundQuery)
        insFuture.addListener(freeTicket, MoreExecutors.directExecutor)
        insFuture
      } catch {
        case t: Throwable =>
          tickets.release()
          throw t
      }
    }.foreach(_.get)
  }
}
