package com.socrata.geocoders

sealed abstract class GeocodingResult
case object SuccessResult extends GeocodingResult
case object InsufficientlyPreciseResult extends GeocodingResult
case object UninterpretableResult extends GeocodingResult

class GeocodingFailure(val message: String, cause: Throwable = null) extends Exception(message, cause)
class GeocodingCredentialsException(val provider: String, val message: String)
  extends Exception(s"$provider credentials failure: $message")
