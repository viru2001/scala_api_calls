package com.org

import sttp.client3._
import scala.concurrent.duration._

object SttpHttpSync {
  def main(args: Array[String]): Unit = {

    val url = "https://dummyjson.com/carts/add"
//val url = "https://fakestoreapi.com/products"
    val invalidJsonBody = """
      {"title": "test product",
      "price": 13.5,
      "description": "lorem ipsum set",
      "image": "https://i.pravatar.cc",
      "category": "electronic"}
    """
    val validJsonBody = """{
      "userId": "1",
      "products": [
        {
          "id": 144,
          "quantity": 4
        },
        {
          "id": 98,
          "quantity": 1
        }
      ]
    }"""

    val headers = Map("Content-Type" -> "application/json")
    val maxRetries = 3
    val initialDelay = 1
    val maxDelay = 5

    retryRequest(url, headers, invalidJsonBody, validJsonBody, maxRetries, initialDelay, maxDelay)
  }

  private def retryRequest(url: String, headers: Map[String, String], invalidJson: String, validJson: String, maxRetries: Int, initialDelay: Int, maxDelay: Int): Unit = {
    val backend = HttpClientSyncBackend()
    for (attempt <- 1 to maxRetries) {
      try {
        val jsonBody = if (attempt == 1) invalidJson else validJson
        println(s"Attempt $attempt: Sending request with JSON: $jsonBody")

        val request = basicRequest
          .post(uri"$url")
          .body(jsonBody)
          .headers(headers)

        val response = request.send(backend)

        if (response.code.isSuccess) {
          println(s"Request successful. Response Code: ${response.code}")
          println(s"Response Body: ${response.body}")
          return // Exit on success
        } else {
          println(s"Unsuccessful response. Code: ${response.code}")
          println(s"Response Body: ${response.body}")
          println( "Retrying...")
        }
      } catch {
        case e: Exception =>
          println(s"Error on attempt $attempt: ${e.getMessage}. Retrying...")
      }

      if (attempt < maxRetries) {
        val delay = calculateDelay(attempt, initialDelay, maxDelay)
        println(s"Waiting ${delay} seconds before next attempt...")
        Thread.sleep(delay)
      }
    }
    println("Request failed after all retries.")
    System.exit(1)
  }

  // Private function to calculate exponential backoff delay
  private def calculateDelay(attempt: Int, initialDelay: Int, maxDelay: Int): Int = {
    val delay = initialDelay * math.pow(2, attempt - 1).toInt
    delay.min(maxDelay)
  }
}
