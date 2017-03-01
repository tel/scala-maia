/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import cats.data.Validated
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
        "latitude",
        Location[RequestMode](
          latitude = true,
          longitude = false
        ),
        resp =>
          Validated.fromOption(
            resp.latitude,
            LookupError.ResponseMissingCRITICAL("latitude")
        )
      ),
      longitude = qm.Atom(
        "longitude",
        Location[RequestMode](
          latitude = false,
          longitude = true
        ),
        resp =>
          Validated.fromOption(
            resp.longitude,
            LookupError.ResponseMissingCRITICAL("longitude")
        )
      )
    )

}
