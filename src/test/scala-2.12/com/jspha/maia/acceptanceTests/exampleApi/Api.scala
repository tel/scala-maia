/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import cats.data.Validated
import com.jspha.maia._
import fs2.Task

final case class Api[M <: Mode](
  getUser: M#Obj[User]
)

object Api {

  type Fm = FetcherMode[Task]

  val fetcher: Fetcher[Task, Api] =
    Api[Fm](
      getUser = Task.now {
        User.fetchCurrent
      }
    )

  private val qm = new QueryMode[Api]

  val q: Query[Api] =
    Api[QueryMode[Api]](
      getUser = qm.Obj[User](
        "getUser",
        req =>
          Api[RequestMode](
            getUser = Some(req)
        ),
        resp =>
          Validated.fromOption(resp.getUser,
                               LookupError.ResponseMissingCRITICAL("getUser")),
        User.q
      )
    )
}
