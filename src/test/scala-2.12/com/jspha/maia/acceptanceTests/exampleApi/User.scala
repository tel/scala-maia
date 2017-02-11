/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import com.jspha.maia.{FetcherMode, _}
import fs2.Task
import fs2.interop.cats._

final case class User[M <: Mode](
  name: M#Atom[String],
  age: M#Atom[Int],
  hometown: M#Obj[City],
  lastKnownLocation: M#Obj[Location]
)

object User {

  type Fm = FetcherMode[Task]

  val fetchCurrent: Fetcher[Task, User] =
    User[Fm](
      name = Task.now("Joseph Abrahamson"),
      age = Task.now(29),
      hometown = Task.now(City.fetchByName("Atlanta")),
      lastKnownLocation = Task.now {
        Fetcher.ofConst(
          Location[ConstantMode](latitude = 0.0, longitude = 0.0)
        )
      }
    )

}
