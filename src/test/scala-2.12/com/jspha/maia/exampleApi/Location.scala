/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.exampleApi

import com.jspha.maia._
import cats._

final case class Location[M <: Mode](
  latitude: M#Atom[Double],
  longitude: M#Atom[Double]
)

object Location {

  def fetchConst(latitude: Double, longitude: Double): Fetcher[Id, Location] =
    Location[Mode.Fetcher[Id]](
      latitude = latitude,
      longitude = longitude
    )

  val q: Query[Location] =
    implicitly[props.HasQuery[Location]].query

  val i: props.Interprets[Id, Location] =
    props.Interprets[Id, Location]

}
