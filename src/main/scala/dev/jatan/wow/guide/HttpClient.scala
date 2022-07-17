package dev.jatan.wow.guide

import java.net.URI
import java.net.http.{HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets

object HttpClient extends Logging {
  private lazy val Client = java.net.http.HttpClient.newHttpClient()

  def get(uri: URI): HttpResponse[String] = {
    log.info(s"[request] $uri")
    Client.send(HttpRequest.newBuilder(uri).build(), HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
  }

  def get(url: String): HttpResponse[String] = get(new URI(url))
}
