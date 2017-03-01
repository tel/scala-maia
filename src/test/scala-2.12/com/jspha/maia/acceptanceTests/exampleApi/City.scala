/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import cats.data.Validated
import com.jspha.maia._
import fs2.Task
import fs2.interop.cats._

final case class City[M <: Mode](
  name: M#Atom[String],
  location: M#Obj[Location]
)

object City {

  type Fm = FetcherMode[Task]

  def atlanta: Fetcher[Task, City] =
    City[Fm](
      name = Task.now("Atlanta"),
      location = Task.now {
        Fetcher.ofConst(
          Location[ConstantMode](
            latitude = 33.7490,
            longitude = 84.3880
          )
        )
      }
    )

  private val qm = new QueryMode[City]

  val q: Query[City] =
    City[QueryMode[City]](
      name = qm.Atom(
        "name",
        City[RequestMode](
          name = true,
          location = None
        ),
        resp =>
          Validated.fromOption(
            resp.name,
            LookupError.ResponseMissingCRITICAL("name")
        )
      ),
      location = qm.Obj[Location](
        "location",
        req =>
          City[RequestMode](
            name = false,
            location = Some(req)
        ),
        resp =>
          Validated.fromOption(
            resp.location,
            LookupError.ResponseMissingCRITICAL("location")
        ),
        Location.q
      )
    )

}
