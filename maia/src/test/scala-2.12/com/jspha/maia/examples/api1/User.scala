/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import com.jspha.maia._
import cats._

final case class User[M <: Dsl](
  name: M#Atom[String],
  age: M#Atom[Int],
  hometown: M#Obj[City],
  lastKnownLocation: M#Obj[Location],
  getId: M#Obj[Identity]
)

object User {

  sealed trait UID
  case object Root extends UID
  case object JosephAbrahamson extends UID

  def fetch(id: UID): Handler[Id, User] = id match {
    case Root =>
      User[form.Handler[Id]](
        name = "Root",
        age = -1,
        hometown = City.atlanta,
        lastKnownLocation = Location.fetchConst(0, 0),
        getId = Identity.fetcher("root")
      )
    case JosephAbrahamson =>
      User[form.Handler[Id]](
        name = "Joseph Abrahamson",
        age = 29,
        hometown = City.atlanta,
        lastKnownLocation = Location.fetchConst(42.3601, 71.0589),
        getId = Identity.fetcher("joseph-abrahamson")
      )
  }

  val req0: Request[User] =
    typelevel.NullRequest[User]

  val q: QueriesAt[User] =
    typelevel.GetQueriesAt[User]

  def runner(req: Request[User]): Response[User] =
    typelevel.RunHandler[Id, User](fetch(Root), req)

}
