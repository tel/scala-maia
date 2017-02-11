/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import com.jspha.maia._
import fs2.Task
import fs2.interop.cats._

final case class City[M <: Mode](
  name: M#Atom[String],
  location: M#Obj[Location]
)

object City {

  type Fm = FetcherMode[Task]

  def fetchByName(name: String): Fetcher[Task, City] =
    name match {
      case "Atlanta" =>
        City[Fm](
          name = Task.now(name),
          location = Task.now {
            Fetcher.ofConst(
              Location[ConstantMode](latitude = 0.0, longitude = 0.0)
            )
          }
        )
    }

}
