/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import com.jspha.maia

import scala.collection.immutable.HashMap

sealed trait Mode {
  type Atom[A]
  type IAtom[I, A]

  type Obj1[A[_ <: Mode]] = Obj[Cardinality.One, A]
  type Obj[M <: Cardinality, A[_ <: Mode]]

  type IObj1[I, A[_ <: Mode]] = IObj[I, Cardinality.One, A]
  type IObj[I, M <: Cardinality, A[_ <: Mode]]
}

object Mode {

  final class Fetcher[F[_], E] extends Mode {
    type Atom[A] = F[Either[E, A]]
    type IAtom[I, A] = I => F[Either[E, A]]
    type Obj[M <: Cardinality, Api[_ <: Mode]] =
      F[Either[E, M#Coll[maia.Fetcher[F, E, Api]]]]
    type IObj[I, M <: Cardinality, Api[_ <: Mode]] =
      I => F[Either[E, M#Coll[maia.Fetcher[F, E, Api]]]]
  }

  final class Query[Super[_ <: Mode], E] extends Mode {

    type Atom[A] = Lookup[Super, E, A]
    type IAtom[I, A] = I => Lookup[Super, E, A]

    trait Obj[M <: Cardinality, Sub[_ <: Mode]] {
      def apply[R](cont: maia.Query[E, Sub] => Lookup[Sub, E, R])
        : Lookup[Super, E, M#Coll[R]]
    }

    trait IObj[I, M <: Cardinality, Sub[_ <: Mode]] {
      def apply[R](ix: I)(cont: maia.Query[E, Sub] => Lookup[Sub, E, R])
        : Lookup[Super, E, M#Coll[R]]
    }
  }

  final class Response[E] extends maia.Mode {
    type Atom[A] = Option[Either[E, A]]
    type IAtom[I, A] = HashMap[I, Either[E, A]]
    type Obj[M <: maia.Cardinality, A[_ <: maia.Mode]] =
      Option[Either[E, M#Coll[maia.Response[E, A]]]]
    type IObj[I, M <: maia.Cardinality, A[_ <: maia.Mode]] =
      HashMap[I, Either[E, M#Coll[maia.Response[E, A]]]]
  }

  sealed trait Request extends maia.Mode {
    type Atom[A] = Boolean
    type IAtom[I, A] = Set[I]
    type Obj[M <: maia.Cardinality, A[_ <: maia.Mode]] =
      Option[maia.Request[A]]
    type IObj[I, M <: maia.Cardinality, A[_ <: maia.Mode]] =
      HashMap[I, maia.Request[A]]
  }

  object Request extends Request

}
