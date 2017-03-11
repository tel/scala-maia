/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import com.jspha.maia

import scala.collection.immutable.HashMap

/**
  * A [[Mode]] is a type signature providing meaning for the various types of
  * field-lookups available in an Api.
  */
trait Mode {

  /**
    * [[Atom]] describes a field which returns a value atomically---no
    * further query is run against the returned value, it's returned wholesale.
    * @tparam A Return type of this field lookup
    */
  type Atom[A]

  /**
    * [[IAtom]] describes an atomic field that is indexed by a single
    * argument. This argument *must* be provided in order to qualify which
    * value is returned
    * @tparam I Index or "argument" to the field
    * @tparam A Return type of this field lookup
    */
  type IAtom[I, A]

  /**
    * [[Obj1]] describes a field returning a single "object" which supports
    * sub-querying.
    * @tparam A Api-schema of the returned "object" values
    */
  type Obj1[A[_ <: Mode]] = Obj[Cardinality.One, A]

  /**
    * [[Obj]] describes a generic "object" field much like [[Obj1]] but it
    * also supports a choice of [[Cardinality]] of the set of object
    * responses returned.
    * @tparam M [[Cardinality]] of the set of objects returned by this field
    *          lookup
    * @tparam A Api-schema of the returned "object" values
    */
  type Obj[M <: Cardinality, A[_ <: Mode]]

  /**
    * [[IObj1]] is similar to [[Obj1]] but allows for indexing or
    * parameterization of the field lookup
    * @tparam I The index or "argument" to this field lookup
    * @tparam A Api-schema of the returned "object" values
    */
  type IObj1[I, A[_ <: Mode]] = IObj[I, Cardinality.One, A]

  /**
    * [[IObj]] is similar to [[Obj]] but allows for indexing or
    * parameterization of the field lookup
    * @tparam I The index or "argument" to this field lookup
    * @tparam M [[Cardinality]] of the set of objects returned by this field
    *          lookup
    * @tparam A Api-schema of the returned "object" values
    */
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
