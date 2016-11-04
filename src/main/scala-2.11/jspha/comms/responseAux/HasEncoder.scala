/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.responseAux

import scala.language.higherKinds
import jspha.comms._
import io.circe._
import shapeless._

trait HasEncoder[Api[_ <: Spec], E] extends Encoder[Response[Api, E]] {
  val encoder: Encoder[Response[Api, E]]
  def apply(a: Response[Api, E]): Json = encoder(a)
}

object HasEncoder {

  implicit def fromGeneric[Api[_ <: Spec], E, Repr <: HList](
      implicit gen: LabelledGeneric.Aux[Response[Api, E], Repr],
      oe: ObjectEncoder[Repr]
  ): HasEncoder[Api, E] =
    new HasEncoder[Api, E] {
      val encoder: Encoder[Response[Api, E]] =
        oe.contramap(gen.to)
    }

}
