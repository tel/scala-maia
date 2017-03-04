/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.exampleApi

import com.jspha.maia._
import fs2.Task

final case class City[M <: Mode](
  name: M#Atom[String],
  location: M#Obj[Location]
)

object City {

  type Fm = FetcherMode[Task]

  def atlanta: Fetcher[Task, City] =
    City[Fm](
      name = Task.now("Atlanta"),
      location = Task.now {
        Location.fetchConst(33.7490, 84.3880)
      }
    )

  val q: Query[City] =
    implicitly[props.HasQuery[City]].query

}
