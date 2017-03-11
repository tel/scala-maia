/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import com.jspha.maia._
import cats._

final case class Location[M <: Mode](
  latitude: M#Atom[Double],
  longitude: M#Atom[Double]
)

object Location {

  def fetchConst(latitude: Double,
                 longitude: Double): Fetcher[Id, Err, Location] =
    Location[Mode.Fetcher[Id, Err]](
      latitude = Right(latitude),
      longitude = Right(longitude)
    )

  val q: Query[Err, Location] =
    implicitly[generic.HasQuery[Err, Location]].query

  val i: generic.Interprets[Id, Err, Location] =
    generic.Interprets[Id, Err, Location]

}
