/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import com.jspha.maia._
import cats._

final case class City[F <: Dsl](
  name: F#Atom[String],
  location: F#Obj[Location]
)

object City {

  def atlanta: Handler[Id, City] =
    City[form.Handler[Id]](
      name = "Atlanta",
      location = Location.fetchConst(33.7490, 84.3880)
    )

  val req0: Request[City] =
    typelevel.NullRequest[City]

  val q: QueriesAt[City] =
    typelevel.GetQueriesAt[City]

  def runner(req: Request[City]): Response[City] =
    typelevel.RunHandler[Id, City](atlanta, req)

}
