package com.socrata.geocoders.caching

import com.netflix.astyanax.Keyspace
import com.netflix.astyanax.model.ColumnFamily
import com.netflix.astyanax.serializers.StringSerializer
import com.rojoma.json.v3.ast.{JNull, JNumber}
import com.rojoma.json.v3.util.JsonUtil
import com.socrata.geocoders.{Address, LatLon}

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

class CassandraCacheClient(keyspace: Keyspace, columnFamilyName: String, cacheTime: FiniteDuration) extends CacheClient {
  val cacheTTL: java.lang.Integer = cacheTime.toSeconds.toInt

  val columnFamily = new ColumnFamily(columnFamilyName,
    StringSerializer.get,
    StringSerializer.get)

  val column = "coords"

  override def lookup(addresses: Seq[Address]): Seq[Option[Option[LatLon]]] = {
    // postcondition: result.length == addresses.length
    val rows = addresses.map(toRowIdentifier)
    val result = keyspace.prepareQuery(columnFamily).
      getRowSlice(rows.asJava).
      withColumnSlice(column).
      execute()
    (addresses, rows).zipped.map { (addr, row) =>
      Option(result.getResult.getRow(row)).flatMap { row =>
        Option(row.getColumns.getStringValue(column, null)).flatMap { col =>
          JsonUtil.parseJson[Either[JNull, (JNumber, JNumber)]](col) match {
            case Right(Right((lat, lon))) => Some(Some(LatLon(lat.toDouble, lon.toDouble)))
            case Right(Left(JNull)) => Some(None)
            case Left(_) => None
          }
        }
      }
    }
  }

  override def cache(addresses: Seq[(Address, Option[LatLon])]): Unit = {
    if(addresses.nonEmpty) {
      val mutation = keyspace.prepareMutationBatch
      for((address, coordinates) <- addresses) {
        val payload: Either[JNull, (JNumber,JNumber)] =
          coordinates match {
            case Some(LatLon(lat, lon)) => Right((JNumber(lat), JNumber(lon)))
            case None                   => Left(JNull)
          }
        mutation.withRow(columnFamily, toRowIdentifier(address)).putColumn(column, JsonUtil.renderJson(payload, pretty=false), cacheTTL)
      }
      mutation.execute()
    }
  }
}
