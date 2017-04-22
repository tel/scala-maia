/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.typelevel

import scala.language.higherKinds
import com.jspha.maia._
import com.jspha.maia.internal.Field
import shapeless._
import shapeless.labelled._
import shapeless.poly._

trait LabelledFiller[D <: Dsl, F <: Poly0, Lf <: HList] {
  type Out <: HList
  def apply(): Out
}

object LabelledFiller {

  type Aux[D <: Dsl, F <: Poly0, Lf <: HList, Out0 <: HList] =
    LabelledFiller[D, F, Lf] { type Out = Out0 }

  implicit def labelledFillerHNil[D <: Dsl, F <: Poly0]
    : Aux[D, F, HNil, HNil] =
    new LabelledFiller[D, F, HNil] {
      type Out = HNil
      def apply() = HNil
    }

  implicit def fillerHConsAtom[D <: Dsl,
                               F <: Poly0,
                               K,
                               A,
                               Lt <: HList,
                               Lft <: HList,
                               As <: ArgSpec,
                               Es <: ErrSpec,
                               S <: Size](
    implicit recur: Aux[D, F, Lft, Lt],
    theField: Case0.Aux[F, FieldType[K, Field.Atom[A, D, As, Es, S]]]
  ): Aux[D,
         F,
         FieldType[K, Field.Atom[A, D, As, Es, S]] :: Lft,
         FieldType[K, D#AtomK[A, As, Es, S]] :: Lt] =
    new LabelledFiller[D, F, FieldType[K, Field.Atom[A, D, As, Es, S]] :: Lft] {
      type Out = FieldType[K, D#AtomK[A, As, Es, S]] :: Lt
      def apply() = field[K](theField().value) :: recur()
    }

  implicit def fillerHConsObj[D <: Dsl,
                              F <: Poly0,
                              K,
                              T[_ <: Dsl],
                              Lt <: HList,
                              Lft <: HList,
                              As <: ArgSpec,
                              Es <: ErrSpec,
                              S <: Size](
    implicit recur: Aux[D, F, Lft, Lt],
    theField: Case0.Aux[F, FieldType[K, Field.Obj[T, D, As, Es, S]]]
  ): Aux[D,
         F,
         FieldType[K, Field.Obj[T, D, As, Es, S]] :: Lft,
         FieldType[K, D#ObjK[T, As, Es, S]] :: Lt] =
    new LabelledFiller[D, F, FieldType[K, Field.Obj[T, D, As, Es, S]] :: Lft] {
      type Out = FieldType[K, D#ObjK[T, As, Es, S]] :: Lt
      def apply() = field[K](theField().value) :: recur()
    }

}
