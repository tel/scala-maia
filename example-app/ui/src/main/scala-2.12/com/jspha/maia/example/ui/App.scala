/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.example.ui

import com.jspha.maia._
import com.jspha.maia.example.api.TopLevel
import fr.hmil.roshttp.HttpRequest
import fr.hmil.roshttp.body.PlainTextBody
import fr.hmil.roshttp.response.SimpleHttpResponse
import io.circe._
import io.circe.parser.decode
import monix.execution.Scheduler.Implicits.global

import scala.util.{Failure, Success}

object App {

  val lk: Query[TopLevel, Nothing, Int] =
    Lookups.lookupCount

  val reqEncoder: Encoder[Request[TopLevel]] =
    ??? // implicitly[RequestEncoder[TopLevel]]

  val respDecoder: Decoder[Response[TopLevel]] =
    ??? // implicitly[ResponseDecoder[TopLevel]]

  def run(request: HttpRequest): Unit =
    request
      .post(PlainTextBody(reqEncoder(lk.request).noSpaces))
      .onComplete({
        case res: Success[SimpleHttpResponse] =>
          val out = decode(res.value.body)(respDecoder).map(lk.handleResponse)
          println(out)
        case _: Failure[SimpleHttpResponse] =>
          println("Houston, we got a problem!")
      })

}
