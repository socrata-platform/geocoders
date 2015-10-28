package com.socrata.geocoders

sealed abstract class GeocodingResult
case object SuccessResult extends GeocodingResult
case object InsufficientlyPreciseResult extends GeocodingResult
case object UninterpretableResult extends GeocodingResult

class GeocodingFailure(val message: String, cause: Throwable = null) extends Exception(message, cause)
class GeocodingCredentialsException(val message: String) extends Exception(message)
