/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.responseAux

import scala.language.higherKinds
import jspha.comms._
import io.circe
import shapeless._

trait Encoder[Api[_ <: Spec]] extends circe.Encoder[Response[Api]]

object Encoder {

  implicit def fromGeneric[Api[_ <: Spec], Repr <: HList](
      implicit gen: LabelledGeneric.Aux[Response[Api], Repr],
      oe: ObjectEncoder[Repr]
  ): circe.Encoder[Response[Api]] =
    oe.contramap(gen.to)

}
