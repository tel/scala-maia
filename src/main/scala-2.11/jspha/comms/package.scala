/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha

import io.circe.Json

import scala.language.higherKinds
import jspha.comms.util.ToWire

package object comms {

  type Request[Api[_ <: Spec]] = Api[RequestSpec]
  type Response[Api[_ <: Spec]] = Api[ResponseSpec]

  implicit class RequestOps[Api[_ <: Spec]](request: Request[Api]) {
    def toWire(implicit f: ToWire[Api]): wire.Request = f(request)
    def asJson(implicit f: ToWire[Api]): Json =
      wire.Request.hasEncoder(toWire)
  }

  implicit class ResponseOps[Api[_ <: Spec]](response: Response[Api]) {

  }

}
