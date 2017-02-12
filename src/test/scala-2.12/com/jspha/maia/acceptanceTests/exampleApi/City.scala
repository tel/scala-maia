/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests.exampleApi

import com.jspha.maia._
import fs2.Task
import fs2.interop.cats._

final case class City[M <: Mode](
  name: M#Atom[String],
  location: M#Obj[Location]
)

object City {

  type Fm = FetcherMode[Task]

  def fetchByName(name: String): Fetcher[Task, City] =
    name match {
      case "Atlanta" =>
        City[Fm](
          name = Task.now(name),
          location = Task.now {
            Fetcher.ofConst(
              Location[ConstantMode](latitude = 0.0, longitude = 0.0)
            )
          }
        )
    }

  private val qm = new QueryMode[City]

  val q: Query[City] =
    City[QueryMode[City]](
      name = qm.Atom(
        City[RequestMode](
          name = true,
          location = None
        ),
        // TODO: This bare get indicates the need for error handling!
        res =>
          res.name match {
            case Some(v) => v
        }
      ),
      location = qm.Obj[Location](
        req =>
          City[RequestMode](
            name = false,
            location = Some(req)
        ),
        // TODO: This bare get indicates the need for error handling!
        resp =>
          resp.location match {
            case Some(r) => r
        },
        Location.q
      )
    )

}
