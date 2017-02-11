/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

class Server[M[_], Api[_ <: Mode]](interpreter: Fetcher[M, Api])(
  implicit interprets: props.Interprets[M, Api]
) extends Handler[M, Api] {

  def apply(request: Request[Api]): M[Response[Api]] =
    interprets(interpreter, request)

}
