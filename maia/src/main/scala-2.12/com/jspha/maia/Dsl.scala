/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

trait Dsl {

  type Atom[A] = AtomK[A, NoArg, NoErr, One]
  type Obj[T[_ <: Dsl]] = ObjK[T, NoArg, NoErr, One]

  type AtomK[A, As <: ArgSpec, Es <: ErrSpec, C <: Size]
  type ObjK[T[_ <: Dsl], As <: ArgSpec, Es <: ErrSpec, C <: Size]

}
