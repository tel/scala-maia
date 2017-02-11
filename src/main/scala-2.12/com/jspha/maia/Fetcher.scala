/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import cats._

object Fetcher {

  def ofConst[M[_]: Monad, Api[_ <: Mode]](c: Constant[Api])(
    implicit fOC: props.FetcherOfConst[M, Api]
  ): Fetcher[M, Api] = fOC(c)

}
