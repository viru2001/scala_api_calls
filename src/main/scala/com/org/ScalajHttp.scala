package com.org

import scalaj.http._
import java.util.concurrent.TimeUnit

object ScalajHttp {
  def main(args: Array[String]): Unit = {

    val url = "https://fakestoreapi.com/products"
    val invalidJsonBody = """{
      "title": "test product",
      "price": 13.5,
      "description": "lorem ipsum set",
      "image": "https://i.pravatar.cc",
      "category": "electronic"
    """
    val validJsonBody = """{
      "title": "test product",
      "price": 13.5,
      "description": "lorem ipsum set",
      "image": "https://i.pravatar.cc",
      "category": "electronic"
    }"""

    val headers = Map("Content-Type" -> "application/json", "Authorization" -> "Bearer token")
    val maxRetries = 3
    val initialDelay = 5
    val maxDelay = 10

    retryRequest(url, headers, invalidJsonBody, validJsonBody, maxRetries, initialDelay, maxDelay)
  }

  private def retryRequest(url: String, headers: Map[String, String], invalidJson: String, validJson: String, maxRetries: Int, initialDelay: Int, maxDelay: Int): Unit = {
    for (attempt <- 1 to maxRetries) {
      try {
        val jsonBody = if (attempt == 1) invalidJson else validJson
        println(s"Attempt $attempt: Sending request with JSON: $jsonBody")

        val response = Http(url)
          .postData(jsonBody)
          .headers(headers)
          .asString

        if (response.code >= 200 && response.code < 300) {
          println(s"Request successful. Response Code: ${response.code}")
          println(s"Response Body: ${response.body}")
          return // Exit on success
        } else {
          println(s"Unsuccessful response. Code: ${response.code}. Retrying...")
        }
      } catch {
        case e: Exception =>
          println(s"Error on attempt $attempt: ${e.getMessage}. Retrying...")
      }

      if (attempt < maxRetries) {
        val delay = calculateDelay(attempt, initialDelay, maxDelay)
        println(s"Waiting ${delay} seconds before next attempt...")
        TimeUnit.SECONDS.sleep(delay)
      }
    }
    println("Request failed after all retries.")
  }

  // Private function to calculate exponential backoff delay
  private def calculateDelay(attempt: Int, initialDelay: Int, maxDelay: Int): Int = {
    val delay = initialDelay * Math.pow(2, attempt - 1).toInt
    Math.min(delay, maxDelay)
  }
}

