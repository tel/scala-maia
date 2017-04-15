/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.example.server

import com.jspha.maia._
import com.jspha.maia.example.api.TopLevel
import com.jspha.maia.http4s.ApiEndpoint
import fs2.interop.cats._
import fs2.{Stream, Task}
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze._
import org.http4s.util.StreamApp
import org.http4s.server.middleware._

object Runtime extends StreamApp {

  val indexService = HttpService {
    case req @ GET -> Root =>
      StaticFile.fromResource("/index.html", Some(req)) match {
        case None => NotFound()
        case Some(f) => Task.now(f)
      }
  }

  val staticResourcesService = HttpService {
    case req @ GET -> path => {
      StaticFile.fromResource(path.toString, Some(req)) match {
        case None => NotFound()
        case Some(f) => Task.now(f)
      }
    }
  }

  val apiService: ApiEndpoint[Task, TopLevel] =
    ApiEndpoint[TopLevel](
      TopLevel[Fields.Fetcher[Task]](
        getCount = Task.now(Right(0))
      )
    )

  def main(args: List[String]): Stream[Task, Unit] = {
    BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(indexService, "/")
      .mountService(staticResourcesService, "/static")
      .mountService(CORS(apiService.service), "/api")
      .serve
  }

}
