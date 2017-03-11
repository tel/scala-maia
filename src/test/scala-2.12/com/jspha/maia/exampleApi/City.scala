/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.exampleApi

import com.jspha.maia._
import cats._

final case class City[M <: Mode](
  name: M#Atom[String],
  location: M#Obj1[Location]
)

object City {

  type Fm = Mode.Fetcher[Id]

  def atlanta: Fetcher[Id, City] =
    City[Fm](
      name = "Atlanta",
      location = Location.fetchConst(33.7490, 84.3880)
    )

  val q: Query[City] =
    implicitly[props.HasQuery[City]].query

  val i: props.Interprets[Id, City] =
    props.Interprets[Id, City]

}
