/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.exampleApi

import com.jspha.maia._
import fs2.Task

final case class Location[M <: Mode](
  latitude: M#Atom[Double],
  longitude: M#Atom[Double]
)

object Location {

  type Fm = FetcherMode[Task]

  def fetchConst(latitude: Double,
                 longitude: Double): Fetcher[Task, Location] =
    Location[Fm](
      latitude = Task.now(latitude),
      longitude = Task.now(longitude)
    )

  val q: Query[Location] =
    implicitly[props.HasQuery[Location]].query

}
