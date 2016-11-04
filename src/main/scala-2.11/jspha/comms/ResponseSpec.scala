/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import cats.data.Xor

import scala.language.higherKinds
import scala.collection.immutable.HashMap

trait ResponseSpec[E] extends Spec {
  type Atomic[P, M <: Cardinality, T] =
    HashMap[P, Xor[E, M#Wrap[T]]]
  type Nested[P, M <: Cardinality, T[_ <: Spec]] =
    HashMap[P, Xor[E, M#Wrap[Response[T, E]]]]
}
