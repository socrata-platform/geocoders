package com.socrata.geocoders.caching

import scala.concurrent.duration._

import java.sql.{Connection, SQLException}
import javax.sql.DataSource
import java.util.concurrent.atomic.AtomicLong

import liquibase.database.jvm.JdbcConnection
import liquibase.resource.ClassLoaderResourceAccessor
import liquibase.Liquibase
import com.rojoma.json.v3.ast.{JNull, JValue}
import com.rojoma.json.v3.util.JsonUtil
import com.rojoma.simplearm.v2._

import com.socrata.geocoders.{InternationalAddress, LatLon}

class PostgresqlCacheClient(dataSource: DataSource,
                            cacheTime: FiniteDuration) extends CacheClient {
  private val cacheTTL = cacheTime.toSeconds.toInt
  private val cleanInterval = 60.seconds.toMillis
  private val nextClean = new AtomicLong(0L)

  private val log = org.slf4j.LoggerFactory.getLogger(classOf[PostgresqlCacheClient])

  private def withConnection[T]()(f: Connection => T): T =
    using(dataSource.getConnection()) { conn =>
      conn.setAutoCommit(false)
      maybeClean(conn)
      val r = f(conn)
      conn.commit()
      r
    }

  private def isDeadlock(e: SQLException): Boolean = {
    e.getSQLState == "40P01"
  }

  private val cleanStatement = "delete from geocode_cache where remove_at < now()"
  private def maybeClean(conn: Connection) = {
    val now = System.currentTimeMillis()
    if(now > nextClean.get) {
      synchronized {
        if(now > nextClean.get) {
          try {
            using(conn.prepareStatement(cleanStatement))(_.executeUpdate())
            conn.commit()
            nextClean.set(System.currentTimeMillis() + cleanInterval)
          } catch {
            case e: SQLException if isDeadlock(e) =>
              // meh whatever, it'll get cleaned up eventually
              conn.rollback()
          }
        }
      }
    }
  }

  private def lookupQuery(n: Int) =
    Iterator.continually("?").take(n).mkString("select address, coords from geocode_cache where address in (", ",", ") and remove_at >= now()")

  override def lookup(addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]] = {
    if(addresses.isEmpty) return Nil

    val outputIndices = addresses.iterator.zipWithIndex.foldLeft(Map.empty[String, List[Int]]) { (acc, addrIdx) =>
      val (addr, idx) = addrIdx
      val key = toRowIdentifier(addr)
      acc + (key -> (idx :: acc.getOrElse(key, Nil)))
    }
    val result = Array.fill(addresses.length)(Option.empty[Option[LatLon]])

    withConnection() { conn =>
      using(conn.prepareStatement(lookupQuery(outputIndices.size))) { stmt =>
        outputIndices.keys.iterator.zipWithIndex.foreach { case (key, i) =>
          stmt.setString(i+1, key)
        }
        using(stmt.executeQuery()) { rs =>
          while(rs.next()) {
            val key = rs.getString(1)
            val point =
              Option(rs.getString(2)) match {
                case Some(jsonText) =>
                  JsonUtil.parseJson[LatLon](jsonText) match {
                    case Right(pt) => Some(Some(pt))
                    case _ => None // present but we couldn't decode; treat as a cache miss
                  }
                case None =>
                  Some(None) // present and cached as no-value
              }
            for(idx <- outputIndices(key)) result(idx) = point
          }
        }
      }
    }

    result
  }

  private val cacheStmt = s"""insert into geocode_cache (address, coords, annotation, remove_at)
                               values (?, ?, ?, (now() + '$cacheTTL seconds' :: interval))
                               on conflict (address) do update set
                                  coords = EXCLUDED.coords,
                                  annotation = EXCLUDED.annotation,
                                  remove_at = EXCLUDED.remove_at"""
  override def cache(addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit = {
    if(addresses.isEmpty) return

    withConnection() { conn =>
      def attemptWrite(): Boolean = {
        try {
          using(conn.prepareStatement(cacheStmt)) { stmt =>
            for((address, (point, ann)) <- addresses) {
              stmt.setString(1, toRowIdentifier(address))
              stmt.setString(2, point.map(JsonUtil.renderJson(_, pretty = false)).orNull)
              stmt.setString(3, JsonUtil.renderJson(ann, pretty = false))
              stmt.addBatch()
            }
            stmt.executeBatch()
          }
          true
        } catch {
          case e: SQLException if isDeadlock(e) =>
            // we drew the short straw, just try again.
            conn.rollback()
            false
        }
      }
      while(!attemptWrite()) {}
    }
  }
}

object PostgresqlCacheClient {
  def init(ds: DataSource) = {
    using(ds.getConnection()) { conn =>
      conn.setAutoCommit(false)
      val liquibase =
        new Liquibase(this.getClass.getPackage.getName.replace('.', '/') + "/migrate.xml",
                      new ClassLoaderResourceAccessor(this.getClass.getClassLoader),
                      new JdbcConnection(conn))
      liquibase.update(conn.getCatalog)
    }
  }
}
