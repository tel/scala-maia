/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.serialization

import scala.language.higherKinds
import com.jspha.maia._
import shapeless._
import shapeless.labelled._

trait RequestEncoder[P <: SerializationParams, T[_ <: Dsl]] {
  def apply(sz: Serializer[P, T], req: Request[T]): P#InjectEff[P#Target]
}

object RequestEncoder {

  def apply[P <: SerializationParams, T[_ <: Dsl]](
    implicit reqEncoder: Lazy[RequestEncoder[P, T]]): RequestEncoder[P, T] =
    reqEncoder.value

  implicit def RequestEncoderG[P <: SerializationParams,
                               T[_ <: Dsl],
                               Ls <: HList,
                               Lr <: HList](
    implicit genericSz: LabelledGeneric.Aux[Serializer[P, T], Ls],
    generic: Generic.Aux[Request[T], Lr],
    worker: Worker[P, Ls, Lr],
    szOps: SerializationOps[P]): RequestEncoder[P, T] =
    (sz: Serializer[P, T], req: Request[T]) =>
      szOps.injectMonad.flatMap(
        worker(szOps, genericSz.to(sz), generic.to(req)))(szOps.obj.inject)

  trait Worker[P <: SerializationParams, Ls <: HList, Lr <: HList] {
    def apply(ops: SerializationOps[P],
              sz: Ls,
              req: Lr): P#InjectEff[Map[String, P#Target]]
  }

  object Worker {
    implicit def WorkerObjHNil[P <: SerializationParams]
      : Worker[P, HNil, HNil] =
      (ops: SerializationOps[P], _: HNil, _: HNil) =>
        ops.injectMonad.pure(Map())

//    implicit def WorkerObjHConsAtomNANEOne[K <: Symbol,
//                                           A,
//                                           As <: ArgSpec,
//                                           Es <: ErrSpec,
//                                           Sz <: Size,
//                                           P <: SerializationParams,
//                                           Ls <: HList,
//                                           Lr <: HList](
////      implicit recur: WorkerObj[P, Ls, Lr],
////      kWitness: Witness.Aux[K]
//    ): Worker[
//      P,
//      FieldType[
//        K,
//        internal.Fix[form.Serializer[P]]#AtomK[A, NoArg, NoErr, One]] :: Ls,
//      form.Request#AtomK[A, NoArg, NoErr, One] :: Lr] = ???
////      (ops: SerializationOps[P],
////       sz: FieldType[K, (Unit, Unit, Section[P, A])] :: Ls,
////       req: Boolean :: Lr) => {
////        val rest: P#InjectEff[Map[String, P#Target]] =
////          recur(ops, sz.tail, req.tail)
////        ops.injectMonad.flatMap(rest) { tail =>
////          if (req.head)
////            ops.injectMonad.map(ops.unit.inject(()))(unit =>
////              tail + (kWitness.value.name -> unit))
////          else
////            ops.injectMonad.pure(tail)
////        }
////      }
//
//    implicit def WorkerObjHConsAtomNANEOpt[K <: Symbol,
//                                           A,
//                                           P <: SerializationParams,
//                                           Ls <: HList,
//                                           Lr <: HList](
//      implicit recur: Worker[P, Ls, Lr],
//      kWitness: Witness.Aux[K]
//    ): Worker[
//      P,
//      FieldType[K, form.Serializer[P]#AtomK[A, NoArg, NoErr, Opt]] :: Ls,
//      form.Request#AtomK[A, NoArg, NoErr, Opt] :: Lr] =
//      (ops: SerializationOps[P],
//       sz: FieldType[K, (Unit, Unit, Section[P, A])] :: Ls,
//       req: Boolean :: Lr) => {
//        val rest: P#InjectEff[Map[String, P#Target]] =
//          recur(ops, sz.tail, req.tail)
//        ops.injectMonad.flatMap(rest) { tail =>
//          if (req.head)
//            ops.injectMonad.map(ops.unit.inject(()))(unit =>
//              tail + (kWitness.value.name -> unit))
//          else
//            ops.injectMonad.pure(tail)
//        }
//      }
//
//    implicit def WorkerObjHConsAtomNANEMany[K <: Symbol,
//                                            A,
//                                            P <: SerializationParams,
//                                            Ls <: HList,
//                                            Lr <: HList](
//      implicit recur: Worker[P, Ls, Lr],
//      kWitness: Witness.Aux[K]
//    ): Worker[
//      P,
//      FieldType[K, form.Serializer[P]#AtomK[A, NoArg, NoErr, Many]] :: Ls,
//      form.Request#AtomK[A, NoArg, NoErr, Many] :: Lr] =
//      (ops: SerializationOps[P],
//       sz: FieldType[K, (Unit, Unit, Section[P, A])] :: Ls,
//       req: Boolean :: Lr) => {
//        val rest: P#InjectEff[Map[String, P#Target]] =
//          recur(ops, sz.tail, req.tail)
//        ops.injectMonad.flatMap(rest) { tail =>
//          if (req.head)
//            ops.injectMonad.map(ops.unit.inject(()))(unit =>
//              tail + (kWitness.value.name -> unit))
//          else
//            ops.injectMonad.pure(tail)
//        }
//      }

  }

}
