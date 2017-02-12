/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import com.jspha.maia._

final case class Location[M <: Mode](
  latitude: M#Atom[Double],
  longitude: M#Atom[Double]
)

object Location {

  private val qm = new QueryMode[Location]
  val q: Query[Location] =
    Location[QueryMode[Location]](
      latitude = qm.Atom(
        Location[RequestMode](
          latitude = true,
          longitude = false
        ),
        // TODO: This bare get indicates the need for error handling!
        // Responses always include the potential for missing data---not only
        // could the library be in error, it could also be a server refusal!
        res =>
          res.latitude match {
            case Some(v) => v
        }
      ),
      longitude = qm.Atom(
        Location[RequestMode](
          latitude = false,
          longitude = true
        ),
        // TODO: This bare get indicates the need for error handling! (above)
        res =>
          res.longitude match {
            case Some(v) => v
        }
      )
    )

}
