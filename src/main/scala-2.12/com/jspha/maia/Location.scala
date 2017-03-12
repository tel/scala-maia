/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._

final case class Location[M <: Mode](
  latitude: M#AtomE[Int, Double],
  longitude: M#AtomE[Nothing, Double]
)

object Location {

  def fetchConst(latitude: Double, longitude: Double): Fetcher[Id, Location] =
    Location[Mode.Fetcher[Id]](
      latitude = Right(latitude),
      longitude = Right(longitude)
    )

  val q: Query[Location] =
    implicitly[generic.HasQuery[Location]].query

  val i: generic.Interprets[Id, Location] =
    generic.Interprets[Id, Location]

}
