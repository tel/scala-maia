/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

/*

  Handlers are the generic representation of Request ~> Response transmission.
  In particular, on the server side a Handler will construct actual Response
  values. On the client side it's more likely to be a caching/forwarding
  layer that corresponds with the aforementioned Server.

 */

trait Handler[M[_], Api[_ <: Mode]] {
  def run(request: Request[Api]): M[Response[Api]]
}
