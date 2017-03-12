/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import cats._
import com.jspha.maia._

final case class TopLevel[M <: Mode](
  name: M#Atom[String],
  age: M#Atom[Int],
  getRoot: M#Obj1[User],
  getUser: M#IObj1[User.Identity, User],
  getAllUsers: M#Obj[Cardinality.Many, User]
)

object TopLevel {

  val fetcher: Fetcher[Id, TopLevel] =
    TopLevel[Mode.Fetcher[Id]](
      name = Right("hello"),
      age = Right(10),
      getRoot = Right(User.fetch(User.Root)),
      getUser = (id: User.Identity) => Right(User.fetch(id)),
      getAllUsers = Right(
        List(
          User.fetch(User.Root),
          User.fetch(User.JosephAbrahamson)
        ))
    )

  val q: Query[TopLevel] =
    implicitly[generic.HasQuery[TopLevel]].query

  val i: generic.Interprets[Id, TopLevel] =
    generic.Interprets[Id, TopLevel]

}
