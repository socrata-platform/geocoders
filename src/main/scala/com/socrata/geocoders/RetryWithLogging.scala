package com.socrata.geocoders

import java.io.IOException

import com.rojoma.json.v3.io.JsonReaderException
import com.socrata.http.client.exceptions.HttpClientException

trait RetryWithLogging {

  val provider: String

  val retryCount: Int

  val log: org.slf4j.Logger

  def fail(message: String): Nothing = {
    log.error(message)
    throw new GeocodingFailure(message)
  }

  def fail(message: String, cause: Throwable): Nothing = {
    log.error(message)
    throw new GeocodingFailure(message, cause)
  }

  def credentialsException(message: String): Nothing = {
    log.warn(message)
    throw new GeocodingCredentialsException(provider, message)
  }

  def retrying[T](action: => T, remainingAttempts: Int = retryCount): T = {
    val failure = try {
      return action
    } catch {
      case e: IOException => e
      case e: HttpClientException => e
      case e: JsonReaderException => e
    }

    log.info(s"Unexpected exception {} while talking to $provider; retrying request...", failure)
    if(remainingAttempts > 0) retrying(action, remainingAttempts - 1)
    else fail("ran out of retries", failure)
  }

}
