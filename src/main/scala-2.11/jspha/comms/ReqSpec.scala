/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import scala.language.higherKinds
import scala.collection.immutable.HashMap

trait ReqSpec extends Spec {
  type Atomic[P, M <: Cardinality, T] =
    Set[P]
  type Nested[P, M <: Cardinality, T[_ <: Spec]] =
    HashMap[P, Request[T]]
}

object ReqSpec {

  def toWire[Api[_ <: Spec]](req: Request[Api])(
      implicit toWire: ToWire[Api]): wire.Request = toWire(req)

  trait ToWire[Api[_ <: Spec]] extends (Request[Api] => wire.Request)

}
