/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha

import io.circe.Json

import scala.language.higherKinds

package object comms {

  type Request[Api[_ <: Spec]] = Api[RequestSpec]
  type Response[Api[_ <: Spec]] = Api[ResponseSpec]

  implicit class RequestOps[Api[_ <: Spec]](req: Request[Api]) {
    def toWire(implicit f: requestAux.ToWire[Api]): wire.Request =
      f(req)
    def asJson(implicit f: requestAux.ToWire[Api]): Json =
      wire.Request.hasEncoder(toWire)
  }

  implicit class ResponseOps[Api[_ <: Spec]](response: Response[Api]) {

  }

  object X {
    import shapeless._
    import shapeless.labelled._
    import jspha.comms.{responseAux => rA}

    val xWit = Witness('x)

    val hnil = implicitly[rA.ObjectEncoder[HNil]]
    val atomic = implicitly[
      rA.ObjectEncoder[
        FieldType[
          xWit.T,
          ResponseSpec#Atomic[Na, Cardinality.Singular, Int]
          ] :: HNil]]

  }

}
