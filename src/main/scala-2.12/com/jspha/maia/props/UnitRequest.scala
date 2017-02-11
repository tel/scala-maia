/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.props

import com.jspha.maia.modes.RequestMode
import jspha.maia.modes.RequestMode

import scala.language.higherKinds
import jspha.maia.simple._
import jspha.maia.simple.modes.RequestMode
import shapeless._

trait UnitRequest[Api[_ <: Mode]] {
  def unit: Request[Api]
}

object UnitRequest {

  implicit def UnitRequestGeneric[Api[_ <: Mode], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Request[Api], Repr],
    worker: Worker[Repr]
  ): UnitRequest[Api] =
    new UnitRequest[Api] {
      def unit: Request[Api] = gen.from(worker.unit)
    }

  trait Worker[T <: HList] {
    def unit: T
  }

  object Worker {

    implicit val WorkerHNil: Worker[HNil] = new Worker[HNil] {
      def unit: HNil = HNil
    }

    implicit def WorkerRecurAtom[K <: Symbol, Api[_ <: Mode], A, T <: HList](
      implicit recur: Worker[T]
    ): Worker[RequestMode.Atom[A] :: T] =
      new Worker[RequestMode.Atom[A] :: T] {
        val unit: RequestMode.Atom[A] :: T = false :: recur.unit
      }

    implicit def WorkerRecurObj[K <: Symbol, Api[_ <: Mode], A, T <: HList](
      implicit recur: Worker[T]
    ): Worker[RequestMode.Obj[A] :: T] =
      new Worker[RequestMode.Obj[A] :: T] {
        val unit: RequestMode.Obj[A] :: T = None :: recur.unit
      }

  }

}
