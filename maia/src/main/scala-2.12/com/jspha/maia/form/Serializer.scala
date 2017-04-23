/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.form

import scala.language.higherKinds
import com.jspha.maia.{Serializer => MaiaSerializer, _}
import com.jspha.maia.serialization._
import shapeless.Lazy

sealed trait Serializer[P <: Section.Params] extends Dsl {
  type AtomK[A, As <: ArgSpec, Es <: ErrSpec, S <: Size] =
    (As#Fold[Section[P, ?], Unit, Any],
     Es#Fold[Section[P, ?], Unit, Any],
     Section[P, A])
  type ObjK[T[_ <: Dsl], As <: ArgSpec, Es <: ErrSpec, S <: Size] =
    (As#Fold[Section[P, ?], Unit, Any],
     Es#Fold[Section[P, ?], Unit, Any],
     Lazy[MaiaSerializer[P, T]])
}
