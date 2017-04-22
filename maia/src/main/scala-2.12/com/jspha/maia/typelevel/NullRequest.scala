/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.typelevel

import scala.language.higherKinds
import com.jspha.maia._
import com.jspha.maia.internal.{Field, Fix}
import shapeless._

import scala.collection.immutable.HashMap

trait NullRequest[T[_ <: Dsl]] {
  def apply(): Request[T]
}

object NullRequest {

  def apply[T[_ <: Dsl]](implicit nullRequest: NullRequest[T]): Request[T] =
    nullRequest()

  implicit def nullRequestG[T[_ <: Dsl], L <: HList, Lf <: HList](
    implicit generic: Generic.Aux[Request[T], L],
    genericFx: Generic.Aux[T[Fix[form.Request]], Lf],
    filler: Filler.Aux[form.Request, FieldSource.type, Lf, L]
  ): NullRequest[T] =
    () => generic.from(filler())

  object FieldSource extends Poly0 {
    implicit def AtomNoArg[A, Es <: ErrSpec, S <: Size]
      : Case0[Field.Atom[A, form.Request, NoArg, Es, S]] =
      at(Field.Atom[A, form.Request, NoArg, Es, S](false))
    implicit def AtomArg[A, Ag, Es <: ErrSpec, S <: Size]
      : Case0[Field.Atom[A, form.Request, HasArg[Ag], Es, S]] =
      at(Field.Atom[A, form.Request, HasArg[Ag], Es, S](Set()))
    implicit def ObjNoArg[T[_ <: Dsl], Ag, Es <: ErrSpec, S <: Size]
      : Case0[Field.Obj[T, form.Request, NoArg, Es, S]] =
      at(Field.Obj[T, form.Request, NoArg, Es, S](None))
    implicit def ObjArg[T[_ <: Dsl], Ag, Es <: ErrSpec, S <: Size]
      : Case0[Field.Obj[T, form.Request, HasArg[Ag], Es, S]] =
      at(Field.Obj[T, form.Request, HasArg[Ag], Es, S](HashMap()))
  }

}
