/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import cats.data.Validated
import com.jspha.maia._
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
      hometown = Task.now(City.atlanta),
      lastKnownLocation = Task.now {
        Fetcher.ofConst(
          Location[ConstantMode](latitude = 0.0, longitude = 0.0)
        )
      }
    )

  private val qm = new QueryMode[User]

  val q: Query[User] =
    User[QueryMode[User]](
      name = qm.Atom(
        "name",
        User[RequestMode](
          name = true,
          age = false,
          hometown = None,
          lastKnownLocation = None
        ),
        resp =>
          Validated.fromOption(
            resp.name,
            LookupError.ResponseMissingCRITICAL("name")
        )
      ),
      age = qm.Atom(
        "age",
        User[RequestMode](
          name = false,
          age = true,
          hometown = None,
          lastKnownLocation = None
        ),
        resp =>
          Validated.fromOption(
            resp.age,
            LookupError.ResponseMissingCRITICAL("age")
        )
      ),
      hometown = qm.Obj[City](
        "hometown",
        req =>
          User[RequestMode](
            name = false,
            age = false,
            hometown = Some(req),
            lastKnownLocation = None
        ),
        resp =>
          Validated.fromOption(
            resp.hometown,
            LookupError.ResponseMissingCRITICAL("hometown")
        ),
        City.q
      ),
      lastKnownLocation = qm.Obj[Location](
        "lastKnownLocation",
        req =>
          User[RequestMode](
            name = false,
            age = false,
            hometown = None,
            lastKnownLocation = Some(req)
        ),
        resp =>
          Validated.fromOption(
            resp.lastKnownLocation,
            LookupError.ResponseMissingCRITICAL("lastKnownLocation")
        ),
        Location.q
      )
    )

}
