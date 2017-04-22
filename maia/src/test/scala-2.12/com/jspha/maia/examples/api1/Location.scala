/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import com.jspha.maia._
import cats.Id

final case class Location[F <: Dsl](
  latitude: F#Atom[Double],
  longitude: F#Atom[Double]
)

object Location {

  def fetchConst(latitude: Double, longitude: Double): Handler[Id, Location] =
    Location[form.Handler[Id]](
      latitude = latitude,
      longitude = longitude
    )

  val req0: Request[Location] =
    typelevel.NullRequest[Location]

  val q: QueriesAt[Location] =
    typelevel.GetQueriesAt[Location]

  def runner(req: Request[Location]): Response[Location] =
    typelevel.RunHandler[Id, Location](fetchConst(0, 0), req)

}
