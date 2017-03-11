/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.exampleApi

import com.jspha.maia._
import cats._

final case class User[M <: Mode](
  name: M#Atom[String],
  age: M#Atom[Int],
  hometown: M#Obj1[City],
  lastKnownLocation: M#Obj1[Location]
)

object User {

  sealed trait Identity
  case object Root extends Identity
  case object JosephAbrahamson extends Identity

  def fetch(id: Identity): Fetcher[Id, User] = id match {
    case Root =>
      User[Mode.Fetcher[Id]](
        name = "Root",
        age = -1,
        hometown = City.atlanta,
        lastKnownLocation = Location.fetchConst(0, 0)
      )
    case JosephAbrahamson =>
      User[Mode.Fetcher[Id]](
        name = "Joseph Abrahamson",
        age = 29,
        hometown = City.atlanta,
        lastKnownLocation = Location.fetchConst(42.3601, 71.0589)
      )
  }

  val q: Query[User] =
    implicitly[props.HasQuery[User]].query

  val i: props.Interprets[Id, User] =
    props.Interprets[Id, User]

}
