package com.org

import sttp.client3._
import sttp.client3.asynchttpclient.future.AsyncHttpClientFutureBackend
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.actor.ActorSystem
import akka.pattern.after

object SttpHttpAsync {
  implicit val system: ActorSystem = ActorSystem("RetrySystem")
  implicit val ec: ExecutionContext = system.dispatcher

  def main(args: Array[String]): Unit = {
    val url = "https://dummyjson.com/carts/add"
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

    retryRequest(url, headers, invalidJsonBody, validJsonBody, maxRetries, initialDelay, maxDelay).onComplete {
      case Success(_) =>
        println("Request completed successfully.")
        system.terminate().onComplete(_ => System.exit(0))
      case Failure(_) =>
        println("Request failed after all retries.")
        system.terminate().onComplete(_ => System.exit(1))
    }
  }

  private def retryRequest(url: String, headers: Map[String, String], invalidJson: String, validJson: String, maxRetries: Int, initialDelay: Int, maxDelay: Int): Future[Unit] = {
    val backend = AsyncHttpClientFutureBackend()
    attemptRequest(url, headers, invalidJson, validJson, maxRetries, initialDelay, maxDelay, 1, backend)
  }

  private def attemptRequest(url: String, headers: Map[String, String], invalidJson: String, validJson: String, maxRetries: Int, initialDelay: Int, maxDelay: Int, attempt: Int, backend: SttpBackend[Future, Any])
                            (implicit system: ActorSystem): Future[Unit] = {
    val jsonBody = if (attempt == 1) invalidJson else validJson
    println(s"Attempt $attempt: Sending request with JSON: $jsonBody")

    val request = basicRequest
      .post(uri"$url")
      .body(jsonBody)
      .headers(headers)

    request.send(backend).flatMap { response =>
      if (response.code.isSuccess) {
        println(s"Request successful. Response Code: ${response.code}")
        println(s"Response Body: ${response.body}")
        Future.successful(())
      } else {
        println(s"Unsuccessful response. Code: ${response.code}")
        println(s"Response Body: ${response.body}")

        if (attempt < maxRetries) {
          val delay = calculateDelay(attempt, initialDelay, maxDelay)
          println(s"Waiting ${delay} seconds before next attempt...")
          after(delay.seconds, system.scheduler)(attemptRequest(url, headers, invalidJson, validJson, maxRetries, initialDelay, maxDelay, attempt + 1, backend))
        } else {
          Future.failed(new Exception("Request failed after all retries."))
        }
      }
    }.recoverWith {
      case e: Exception =>
        println(s"Error on attempt $attempt: ${e.getMessage}. Retrying...")
        if (attempt < maxRetries) {
          val delay = calculateDelay(attempt, initialDelay, maxDelay)
          println(s"Waiting ${delay} seconds before next attempt...")
          after(delay.seconds, system.scheduler)(attemptRequest(url, headers, invalidJson, validJson, maxRetries, initialDelay, maxDelay, attempt + 1, backend))
        } else {
          Future.failed(new Exception("Request failed after all retries."))
        }
    }
  }

  // Private function to calculate exponential backoff delay
  private def calculateDelay(attempt: Int, initialDelay: Int, maxDelay: Int): Int = {
    val delay = initialDelay * math.pow(2, attempt - 1).toInt
    delay.min(maxDelay)
  }
}
