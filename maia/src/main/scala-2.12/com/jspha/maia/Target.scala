/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

sealed trait Target {
  type Fold[IfAtomic[_] <: Up, IfNested[_[_ <: Dsl]] <: Up, Up] <: Up
}

sealed trait Atomic[A] extends Target {
  type Fold[IfAtomic[_] <: Up, IfNested[_[_ <: Dsl]] <: Up, Up] =
    IfAtomic[A]
}

sealed trait Nested[T[_ <: Dsl]] extends Target {
  type Fold[IfAtomic[A] <: Up, IfNested[_[_ <: Dsl]] <: Up, Up] =
    IfNested[T]
}
