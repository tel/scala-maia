/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.typelevel

import scala.language.higherKinds
import cats._
import cats.implicits._
import com.jspha.maia._
import com.jspha.maia.internal._
import shapeless._

import scala.collection.immutable.HashMap

trait RunHandler[F[_], T[_ <: Dsl]] {
  def apply(h: Handler[F, T], r: Request[T]): F[Response[T]]
}

object RunHandler {

  def apply[F[_], T[_ <: Dsl]](handler: Handler[F, T], request: Request[T])(
    implicit runHandler: Lazy[RunHandler[F, T]]): F[Response[T]] =
    runHandler.value(handler, request)

  implicit def runHandlerG[F[_],
                           T[_ <: Dsl],
                           Lh <: HList,
                           Lrq <: HList,
                           Lrs <: HList,
                           Lrsf <: HList](
    implicit genericH: Generic.Aux[Handler[F, T], Lh],
    genericRq: Generic.Aux[Request[T], Lrq],
    genericRs: Generic.Aux[Response[T], Lrs],
    genericRsf: Generic.Aux[T[Fix[form.Response]], Lrsf],
    runner: Runner.Aux[F, T, Lrsf, Lh, Lrq, Lrs],
    functor: Functor[F]
  ): RunHandler[F, T] =
    (h: Handler[F, T], r: Request[T]) => {
      val x: F[Lrs] = runner(genericH.to(h), genericRq.to(r))
      functor.map(x)(genericRs.from)
    }

  trait Runner[F[_], T[_ <: Dsl], Lrsf <: HList] {
    type Lh <: HList
    type Lrq <: HList
    type Lrs <: HList
    def apply(h: Lh, r: Lrq): F[Lrs]
  }

  object Runner {

    type Aux[F[_],
             T[_ <: Dsl],
             Lrsf <: HList,
             Lh0 <: HList,
             Lrq0 <: HList,
             Lrs0 <: HList] =
      Runner[F, T, Lrsf] {
        type Lh = Lh0
        type Lrq = Lrq0
        type Lrs = Lrs0
      }

    implicit def runnerHnil[F[_], T[_ <: Dsl]](
      implicit A: Applicative[F]
    ): Aux[F, T, HNil, HNil, HNil, HNil] =
      new Runner[F, T, HNil] {
        type Lh = HNil
        type Lrq = HNil
        type Lrs = HNil

        def apply(h: HNil, r: HNil): F[HNil] =
          A.pure(HNil)
      }

    implicit def runnerHConsAtom[F[_],
                                 T[_ <: Dsl],
                                 A,
                                 As <: ArgSpec,
                                 Es <: ErrSpec,
                                 S <: Size,
                                 Lfz0 <: HList,
                                 Lh0 <: HList,
                                 Lrq0 <: HList,
                                 Lrs0 <: HList](
      implicit recur: Aux[F, T, Lfz0, Lh0, Lrq0, Lrs0],
      applicative: Applicative[F],
      run1: AtomRun1[F, A, As, Es, S]
    ): Aux[F,
           T,
           Field.Atom[A, form.Response, As, Es, S] :: Lfz0,
           form.Handler[F]#AtomK[A, As, Es, S] :: Lh0,
           form.Request#AtomK[A, As, Es, S] :: Lrq0,
           form.Response#AtomK[A, As, Es, S] :: Lrs0] =
      new Runner[F, T, Field.Atom[A, form.Response, As, Es, S] :: Lfz0] {
        type Lh = form.Handler[F]#AtomK[A, As, Es, S] :: Lh0
        type Lrq = form.Request#AtomK[A, As, Es, S] :: Lrq0
        type Lrs = form.Response#AtomK[A, As, Es, S] :: Lrs0

        def apply(h: Lh, r: Lrq): F[Lrs] = {
          val head: F[Field.Atom[A, form.Response, As, Es, S]] =
            run1(h.head, r.head)
          val tail = recur(h.tail, r.tail)
          applicative.map2(head, tail) { case (hd, tl) => hd.value :: tl }
        }
      }

    trait AtomRun1[F[_], A, As <: ArgSpec, Es <: ErrSpec, S <: Size] {
      def apply(
        h: form.Handler[F]#AtomK[A, As, Es, S],
        r: form.Request#AtomK[A, As, Es, S]
      ): F[Field.Atom[A, form.Response, As, Es, S]]
    }

    object AtomRun1 {

      implicit def NANE[F[_], A, S <: Size](
        implicit F: Applicative[F]): AtomRun1[F, A, NoArg, NoErr, S] =
        (h: F[S#Coll[A]], r: Boolean) =>
          if (r)
            F.map(h)(x =>
              Field.Atom[A, form.Response, NoArg, NoErr, S](Some(x)))
          else
            F.pure(Field.Atom[A, form.Response, NoArg, NoErr, S](None))

      implicit def NAE[F[_], A, E, S <: Size](
        implicit F: Applicative[F]): AtomRun1[F, A, NoArg, HasErr[E], S] =
        (h: F[Either[E, S#Coll[A]]], r: Boolean) =>
          if (r)
            F.map(h)(x =>
              Field.Atom[A, form.Response, NoArg, HasErr[E], S](Some(x)))
          else
            F.pure(Field.Atom[A, form.Response, NoArg, HasErr[E], S](None))

      implicit def ANE[F[_], A, Ag, S <: Size](
        implicit F: Applicative[F]): AtomRun1[F, A, HasArg[Ag], NoErr, S] =
        (h: Ag => F[S#Coll[A]], r: Set[Ag]) => {
          val fPairs: F[List[(Ag, S#Coll[A])]] =
            Traverse[List].traverse(r.toList) { (ag: Ag) =>
              F.map(h(ag))(x => (ag, x))
            }
          F.map(fPairs)(
            pairs =>
              Field.Atom[A, form.Response, HasArg[Ag], NoErr, S](
                HashMap(pairs: _*)))
        }

      implicit def AE[F[_], A, E, Ag, S <: Size](
        implicit F: Applicative[F]): AtomRun1[F, A, HasArg[Ag], HasErr[E], S] =
        (h: Ag => F[Either[E, S#Coll[A]]], r: Set[Ag]) => {
          val fPairs: F[List[(Ag, Either[E, S#Coll[A]])]] =
            Traverse[List].traverse(r.toList) { (ag: Ag) =>
              F.map(h(ag))(x => (ag, x))
            }
          F.map(fPairs)(
            pairs =>
              Field.Atom[A, form.Response, HasArg[Ag], HasErr[E], S](
                HashMap(pairs: _*)))
        }

    }

    // Obj

    implicit def runnerHConsObj[F[_],
                                T[_ <: Dsl],
                                U[_ <: Dsl],
                                As <: ArgSpec,
                                Es <: ErrSpec,
                                S <: Size,
                                Lfz0 <: HList,
                                Lh0 <: HList,
                                Lrq0 <: HList,
                                Lrs0 <: HList](
      implicit recur: Aux[F, T, Lfz0, Lh0, Lrq0, Lrs0],
      applicative: Applicative[F],
      run1: ObjRun1[F, U, As, Es, S]
    ): Aux[F,
           T,
           Field.Obj[U, form.Response, As, Es, S] :: Lfz0,
           form.Handler[F]#ObjK[U, As, Es, S] :: Lh0,
           form.Request#ObjK[U, As, Es, S] :: Lrq0,
           form.Response#ObjK[U, As, Es, S] :: Lrs0] =
      new Runner[F, T, Field.Obj[U, form.Response, As, Es, S] :: Lfz0] {
        type Lh = form.Handler[F]#ObjK[U, As, Es, S] :: Lh0
        type Lrq = form.Request#ObjK[U, As, Es, S] :: Lrq0
        type Lrs = form.Response#ObjK[U, As, Es, S] :: Lrs0

        def apply(h: Lh, r: Lrq): F[Lrs] = {
          val head: F[Field.Obj[U, form.Response, As, Es, S]] =
            run1(h.head, r.head)
          val tail = recur(h.tail, r.tail)
          applicative.map2(head, tail) { case (hd, tl) => hd.value :: tl }
        }
      }

    trait ObjRun1[F[_], T[_ <: Dsl], As <: ArgSpec, Es <: ErrSpec, S <: Size] {
      def apply(
        h: form.Handler[F]#ObjK[T, As, Es, S],
        r: form.Request#ObjK[T, As, Es, S]
      ): F[Field.Obj[T, form.Response, As, Es, S]]
    }

    object ObjRun1 {

      implicit def NANE[F[_], T[_ <: Dsl], S <: Size](
        implicit F: Monad[F],
        collOps: Size.Ops[S],
        recur: Lazy[RunHandler[F, T]]): ObjRun1[F, T, NoArg, NoErr, S] =
        (h: F[S#Coll[Handler[F, T]]], r: Option[Request[T]]) =>
          r match {
            case None =>
              F.pure(Field.Obj[T, form.Response, NoArg, NoErr, S](None))
            case Some(subReq) =>
              F.flatMap(h) { handlers =>
                val fResps: F[S#Coll[Response[T]]] =
                  collOps.traversable.traverse(handlers) { handler =>
                    recur.value(handler, subReq)
                  }
                F.map(fResps)(resps =>
                  Field.Obj[T, form.Response, NoArg, NoErr, S](Some(resps)))
              }
        }

      implicit def NAE[F[_], T[_ <: Dsl], E, S <: Size](
        implicit F: Monad[F],
        collOps: Size.Ops[S],
        recur: Lazy[RunHandler[F, T]]): ObjRun1[F, T, NoArg, HasErr[E], S] =
        (h: F[Either[E, S#Coll[Handler[F, T]]]], r: Option[Request[T]]) =>
          r match {
            case None =>
              F.pure(Field.Obj[T, form.Response, NoArg, HasErr[E], S](None))
            case Some(subReq) =>
              F.flatMap(h) {
                case Left(err) =>
                  F.pure(
                    Field.Obj[T, form.Response, NoArg, HasErr[E], S](
                      Some(Left(err))))
                case Right(handlers) =>
                  val fResps: F[S#Coll[Response[T]]] =
                    collOps.traversable.traverse(handlers) { handler =>
                      recur.value(handler, subReq)
                    }
                  F.map(fResps)(
                    resps =>
                      Field.Obj[T, form.Response, NoArg, HasErr[E], S](
                        Some(Right(resps))))
              }
        }

      implicit def ANE[F[_], T[_ <: Dsl], Ag, S <: Size](
        implicit F: Monad[F],
        collOps: Size.Ops[S],
        recur: Lazy[RunHandler[F, T]]): ObjRun1[F, T, HasArg[Ag], NoErr, S] =
        (h: Ag => F[S#Coll[Handler[F, T]]], r: HashMap[Ag, Request[T]]) => {

          val resultPairs: F[List[(Ag, S#Coll[Response[T]])]] =
            Traverse[List].traverse(r.toList) {
              case (ag, req) =>
                F.flatMap(h(ag)) { handlers =>
                  val fResps: F[S#Coll[Response[T]]] =
                    collOps.traversable.traverse(handlers) { handler =>
                      recur.value(handler, req)
                    }
                  F.map(fResps)(resps => (ag, resps))
                }
            }

          F.map(resultPairs)(y =>
            Field.Obj[T, form.Response, HasArg[Ag], NoErr, S](HashMap(y: _*)))

        }

      implicit def AE[F[_], T[_ <: Dsl], Ag, E, S <: Size](
        implicit F: Monad[F],
        collOps: Size.Ops[S],
        recur: Lazy[RunHandler[F, T]])
        : ObjRun1[F, T, HasArg[Ag], HasErr[E], S] =
        (h: Ag => F[Either[E, S#Coll[Handler[F, T]]]],
         r: HashMap[Ag, Request[T]]) => {

          val resultPairs: F[List[(Ag, Either[E, S#Coll[Response[T]]])]] =
            Traverse[List].traverse(r.toList) {
              case (ag, req) =>
                F.flatMap(h(ag)) {
                  case Left(err) =>
                    F.pure((ag, Left(err)))
                  case Right(handlers) =>
                    val fResps: F[S#Coll[Response[T]]] =
                      collOps.traversable.traverse(handlers) { handler =>
                        recur.value(handler, req)
                      }
                    F.map(fResps)(resps => (ag, Right(resps)))
                }
            }

          F.map(resultPairs)(
            y =>
              Field.Obj[T, form.Response, HasArg[Ag], HasErr[E], S](
                HashMap(y: _*)))

        }

    }

  }

}
