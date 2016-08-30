package com.socrata.geocoders

import com.rojoma.json.v3.ast.{JString, JObject, JValue}
import com.rojoma.json.v3.codec.{DecodeError, JsonDecode, JsonEncode}
import com.rojoma.json.v3.util.AutomaticJsonCodecBuilder

import scala.collection.immutable.SortedMap

case class InternationalAddress(address: Option[String],
                                locality: Option[String],
                                subregion: Option[String],
                                region: Option[String],
                                postalCode: Option[String],
                                country: String)

object InternationalAddress {
  // this encoder produces a _canonical_ encoding of the
  // address -- in particular, the fields are sorted.
  implicit object Codec extends JsonEncode[InternationalAddress] with JsonDecode[InternationalAddress] {
    override def encode(addr: InternationalAddress): JValue = {
      val InternationalAddress(address, locality, subregion, region, postalCode, country) = addr
      val intermediate = SortedMap.newBuilder[String, String]
      address.foreach(intermediate += "address" -> _)
      locality.foreach(intermediate += "locality" -> _)
      subregion.foreach(intermediate += "subregion" -> _)
      region.foreach(intermediate += "region" -> _)
      postalCode.foreach(intermediate += "postal_code" -> _)
      intermediate += "country" -> country
      JsonEncode.toJValue(intermediate.result())
    }
    override def decode(x: JValue): Either[DecodeError, InternationalAddress] =
      x match {
        case JObject(fields) =>
          // this has to be a function, not a def, because we're returning from
          // decode from inside of it on error.
          val extractStr = (name: String) => fields.get(name).map {
            case JString(s) => s
            case other => return Left(DecodeError.InvalidType(expected = JString, got = other.jsonType).prefix(name))
          }
          Right(InternationalAddress(
            address = extractStr("address"),
            locality = extractStr("locality"),
            subregion = extractStr("subregion"),
            region = extractStr("region"),
            postalCode = extractStr("postal_code"),
            country = extractStr("country").getOrElse {
              return Left(DecodeError.MissingField("country"))
            }
          ))
        case other =>
          Left(DecodeError.InvalidType(expected = JObject, got = other.jsonType))
      }
  }

  @deprecated(message = "Confusing syntax; use create(.) instead.", since = "2.0.2")
  def apply(address: Option[String],
            locality: Option[String],
            subregion: Option[String],
            region: Option[String],
            postalCode: Option[String],
            country: Option[String]): Option[InternationalAddress] = {
    create(address, locality, subregion, region, postalCode, country)
  }

  // filter out empty strings to None and default country to US
  def create(address: Option[String],
             locality: Option[String],
             subregion: Option[String],
             region: Option[String],
             postalCode: Option[String],
             country: Option[String]): Option[InternationalAddress] = {
    if (Seq(address, locality, subregion, region, postalCode, country).forall(_.isEmpty)) None
    else Some(new InternationalAddress(
      address.filter(_.nonEmpty),
      locality.filter(_.nonEmpty),
      subregion.filter(_.nonEmpty),
      region.filter(_.nonEmpty),
      postalCode.filter(_.nonEmpty),
      country.filter(_.nonEmpty).getOrElse("US")))
  }
}

@deprecated(message = "Unused; use InternationalAddress.create(.) instead.", since = "2.0.2")
object USAddress {
  def apply(address: Option[String],
            city: Option[String],
            state: Option[String],
            zip: Option[String]): Option[InternationalAddress] = {
    InternationalAddress.create(address, city, None, state, zip, None)
  }
}

case class LatLon(lat: Double, lon: Double)

object LatLon {
  implicit val llCodec = AutomaticJsonCodecBuilder[LatLon]
}
