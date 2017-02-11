/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha

import io.circe.Json
import jspha.comms.requestAux.WireRequest

import scala.language.higherKinds

package object comms {

  type Request[Api[_ <: Spec]] = Api[RequestSpec]
  type Response[Api[_ <: Spec], E] = Api[ResponseSpec[E]]

  implicit class RequestOps[Api[_ <: Spec]](request: Request[Api]) {
    def toWire(implicit f: requestAux.ToWire[Api]): WireRequest =
      f(request)
    def asJson(implicit f: requestAux.ToWire[Api]): Json =
      WireRequest.hasEncoder(toWire)
  }

  implicit class ResponseOps[Api[_ <: Spec], E](response: Response[Api, E]) {
    def asJson(implicit e: responseAux.HasEncoder[Api, E]): Json =
      e(response)
  }

}
