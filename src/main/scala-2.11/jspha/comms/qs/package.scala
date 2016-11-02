/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import scala.language.higherKinds
import io.circe.Encoder

package object qs {

  type Request[Api[_ <: Qs]] = Api[ReqS]
  type Response[Api[_ <: Qs]] = Api[RespS]

  implicit def buildRequestEncoder[Api[_ <: Qs]: ReqS.ToWire]
    : Encoder[Request[Api]] =
    Encoder[wire.Request].contramap(ReqS.toWire[Api])

}
