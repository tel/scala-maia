/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.exampleApi

import com.jspha.maia._

final case class Location[M <: Mode](
  latitude: M#Atom[Double],
  longitude: M#Atom[Double]
)

object Location {

  val q: Query[Location] =
    implicitly[props.HasQuery[Location]].query

}
