/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.exampleApi

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

  val q: Query[Api] =
    implicitly[props.HasQuery[Api]].query

}
