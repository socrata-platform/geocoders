package com.socrata.geocoders.caching

import com.netflix.astyanax.Keyspace
import com.netflix.astyanax.model.ColumnFamily
import com.netflix.astyanax.serializers.StringSerializer
import com.rojoma.json.v3.io.{CompactJsonWriter, JsonReader}
import com.rojoma.json.v3.matcher.{FirstOf, Variable, PArray}
import com.rojoma.json.v3.ast.{JNull, JValue}
import com.socrata.geocoders.{InternationalAddress, LatLon}

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

class CassandraCacheClient(keyspace: Keyspace, columnFamilyName: String, cacheTime: FiniteDuration) extends CacheClient {
  val cacheTTL: java.lang.Integer = cacheTime.toSeconds.toInt

  val columnFamily = new ColumnFamily(columnFamilyName,
    StringSerializer.get,
    StringSerializer.get)

  val column = "coords"

  val latLon = Variable[LatLon]()
  val annotation = Variable[JValue]()
  val Pattern = PArray(
    FirstOf(latLon, JNull),
    annotation
  )

  override def lookup(addresses: Seq[InternationalAddress]): Seq[Option[Option[LatLon]]] = {
    // postcondition: result.length == addresses.length
    val rows = addresses.map(toRowIdentifier)
    val result = keyspace.prepareQuery(columnFamily).
      getRowSlice(rows.asJava).
      withColumnSlice(column).
      execute()
    (addresses, rows).zipped.map { (addr, row) =>
      Option(result.getResult.getRow(row)).flatMap { row =>
        Option(row.getColumns.getStringValue(column, null)).flatMap { col =>
          JsonReader.fromString(col) match {
            case Pattern(res) =>
              Some(latLon.get(res))
            case _ =>
              None
          }
        }
      }
    }
  }

  override def cache(addresses: Seq[(InternationalAddress, (Option[LatLon], JValue))]): Unit = {
    if(addresses.nonEmpty) {
      val mutation = keyspace.prepareMutationBatch
      for((address, (point, ann)) <- addresses) {
        mutation.withRow(columnFamily, toRowIdentifier(address)).putColumn(column, CompactJsonWriter.toString(Pattern.generate(latLon :=? point, annotation := ann)), cacheTTL)
      }
      mutation.execute()
    }
  }
}
