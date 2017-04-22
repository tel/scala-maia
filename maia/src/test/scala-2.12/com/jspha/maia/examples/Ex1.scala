/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples

import com.jspha.maia._

object Ex1 {

  final case class Loc[F <: Dsl](
    lat: F#Atom[Double],
    long: F#Atom[Double]
  )

  val latReq: Request[Loc] =
    Loc[form.Request](
      lat = true,
      long = false
    )

  val longReq: Request[Loc] =
    Loc[form.Request](
      lat = false,
      long = true
    )

//  val mergedReq: Request[Loc] =
//    typelevel.MergeReqs[Loc](latReq, longReq)

  val locResp: Loc[form.Response] =
    Loc[form.Response](
      lat = Some(0.0),
      long = Some(0.0)
    )

  type Id[A] = A

  val locFetcher: Handler[Id, Loc] =
    Loc[form.Handler[Id]](
      lat = 0.0,
      long = 0.0
    )

}
