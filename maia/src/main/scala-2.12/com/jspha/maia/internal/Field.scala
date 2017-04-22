/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.internal

import com.jspha.maia.{ArgSpec, Dsl, ErrSpec, Size}

import scala.language.higherKinds

sealed trait Field[D <: Dsl] {
  type Out
}

object Field {

  final case class Atom[A, D <: Dsl, As <: ArgSpec, Es <: ErrSpec, C <: Size](
    value: D#AtomK[A, As, Es, C])
      extends Field[D] {
    type Out = D#AtomK[A, As, Es, C]
  }

  final case class Obj[T[_ <: Dsl], D <: Dsl, As <: ArgSpec, Es <: ErrSpec,
  C <: Size](value: D#ObjK[T, As, Es, C])
      extends Field[D] {
    type Out = D#ObjK[T, As, Es, C]
  }

}
