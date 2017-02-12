/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

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
      hometown = Task.now(City.fetchByName("Atlanta")),
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
        User[RequestMode](
          name = true,
          age = false,
          hometown = None,
          lastKnownLocation = None
        ),
        // TODO: This bare get indicates the need for error handling!
        res =>
          res.name match {
            case Some(v) => v
        }
      ),
      age = qm.Atom(
        User[RequestMode](
          name = false,
          age = true,
          hometown = None,
          lastKnownLocation = None
        ),
        // TODO: This bare get indicates the need for error handling!
        res =>
          res.age match {
            case Some(v) => v
        }
      ),
      hometown = qm.Obj[City](
        req =>
          User[RequestMode](
            name = false,
            age = false,
            hometown = Some(req),
            lastKnownLocation = None
        ),
        // TODO: This bare get indicates the need for error handling!
        resp =>
          resp.hometown match {
            case Some(r) => r
        },
        City.q
      ),
      lastKnownLocation = qm.Obj[Location](
        req =>
          User[RequestMode](
            name = false,
            age = false,
            hometown = None,
            lastKnownLocation = Some(req)
        ),
        // TODO: This bare get indicates the need for error handling!
        resp =>
          resp.lastKnownLocation match {
            case Some(r) => r
        },
        Location.q
      )
    )

}
