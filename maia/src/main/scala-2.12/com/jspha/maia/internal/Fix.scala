/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.internal

import com.jspha.maia._

import scala.language.higherKinds

sealed trait Fix[D <: Dsl] extends Dsl {
  type AtomK[A, As <: ArgSpec, Es <: ErrSpec, C <: Size] =
    Field.Atom[A, D, As, Es, C]
  type ObjK[T[_ <: Dsl], As <: ArgSpec, Es <: ErrSpec, C <: Size] =
    Field.Obj[T, D, As, Es, C]
}
