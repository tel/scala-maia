/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.typelevel

import scala.language.higherKinds
import com.jspha.maia._
import com.jspha.maia.internal.Field
import shapeless._
import shapeless.poly._

trait Filler[D <: Dsl, F <: Poly0, Lf <: HList] {
  type Out <: HList
  def apply(): Out
}

object Filler {

  type Aux[D <: Dsl, F <: Poly0, Lf <: HList, Out0 <: HList] =
    Filler[D, F, Lf] { type Out = Out0 }

  implicit def fillerHNil[D <: Dsl, F <: Poly0]: Aux[D, F, HNil, HNil] =
    new Filler[D, F, HNil] {
      type Out = HNil
      def apply() = HNil
    }

  implicit def fillerHConsAtom[D <: Dsl,
                               F <: Poly0,
                               A,
                               Lt <: HList,
                               Lft <: HList,
                               As <: ArgSpec,
                               Es <: ErrSpec,
                               S <: Size](
    implicit recur: Aux[D, F, Lft, Lt],
    field: Case0.Aux[F, Field.Atom[A, D, As, Es, S]]
  )
    : Aux[D,
          F,
          Field.Atom[A, D, As, Es, S] :: Lft,
          D#AtomK[A, As, Es, S] :: Lt] =
    new Filler[D, F, Field.Atom[A, D, As, Es, S] :: Lft] {
      type Out = D#AtomK[A, As, Es, S] :: Lt
      def apply() = field().value :: recur()
    }

  implicit def fillerHConsObj[D <: Dsl,
                              F <: Poly0,
                              T[_ <: Dsl],
                              Lt <: HList,
                              Lft <: HList,
                              As <: ArgSpec,
                              Es <: ErrSpec,
                              S <: Size](
    implicit recur: Aux[D, F, Lft, Lt],
    field: Case0.Aux[F, Field.Obj[T, D, As, Es, S]]
  ): Aux[D, F, Field.Obj[T, D, As, Es, S] :: Lft, D#ObjK[T, As, Es, S] :: Lt] =
    new Filler[D, F, Field.Obj[T, D, As, Es, S] :: Lft] {
      type Out = D#ObjK[T, As, Es, S] :: Lt
      def apply() = field().value :: recur()
    }

}
