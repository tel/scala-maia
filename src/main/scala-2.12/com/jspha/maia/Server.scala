/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

abstract class Server[M[_], Api[_ <: Mode]](interpreter: Interpreter[M, Api])
    extends Handler[M, Api]

/*

  TODO: Implement the Server.

  For each "requested" field in the Api, we pass the request through the
  given Interpreter, flattening each field request's M layer monadically. At
  the leaves, we'll obtain result values which we can reassemble into a
  response.

 */
