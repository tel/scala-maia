/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import scala.language.higherKinds

trait Qs {
  type Atomic[P, M <: Mult, T]
  type Nested[P, M <: Mult, T[_ <: Qs]]

  final type Atomic1[T] = Atomic[NoParam, Mult.One, T]
  final type Nested1[T[_ <: Qs]] = Nested[NoParam, Mult.One, T]

  final type A[P, M <: Mult, T] = Atomic[P, M, T]
  final type N[P, M <: Mult, T[_ <: Qs]] = Nested[P, M, T]

  final type A1[T] = Atomic1[T]
  final type N1[T[_ <: Qs]] = Nested1[T]
}
