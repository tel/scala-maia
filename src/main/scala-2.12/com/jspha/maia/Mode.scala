/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats.Apply

import scala.language.higherKinds
import com.jspha.maia

import scala.collection.immutable.HashMap

/**
  * A [[Mode]] is a type signature providing meaning for the various types of
  * field-lookups available in an Api.
  */
trait Mode {

  /**
    * [[AtomE]] describes a field which returns a value atomically---no
    * further query is run against the returned value, it's returned wholesale.
    *
    * @tparam E Possible error type
    * @tparam A Return type of this field lookup
    */
  type AtomE[E, A]

  type Atom[A] = AtomE[Nothing, A]

  /**
    * [[IAtomE]] describes an atomic field that is indexed by a single
    * argument. This argument *must* be provided in order to qualify which
    * value is returned
    *
    * @tparam I Index or "argument" to the field
    * @tparam A Return type of this field lookup
    */
  type IAtomE[I, E, A]

  type IAtom[I, A] = IAtomE[I, Nothing, A]

  /**
    * [[ObjE1]] describes a field returning a single "object" which supports
    * sub-querying.
    *
    * @tparam A Api-schema of the returned "object" values
    */
  type ObjE1[E, A[_ <: Mode]] = ObjE[Cardinality.One, E, A]

  type Obj1[A[_ <: Mode]] = ObjE[Cardinality.One, Nothing, A]

  /**
    * [[ObjE]] describes a generic "object" field much like [[ObjE1]] but it
    * also supports a choice of [[Cardinality]] of the set of object
    * responses returned.
    *
    * @tparam M [[Cardinality]] of the set of objects returned by this field
    *          lookup
    * @tparam A Api-schema of the returned "object" values
    */
  type ObjE[M <: Cardinality, E, A[_ <: Mode]]

  type Obj[M <: Cardinality, A[_ <: Mode]] = ObjE[M, Nothing, A]

  /**
    * [[IObjE1]] is similar to [[ObjE1]] but allows for indexing or
    * parameterization of the field lookup
    *
    * @tparam I The index or "argument" to this field lookup
    * @tparam A Api-schema of the returned "object" values
    */
  type IObjE1[I, E, A[_ <: Mode]] = IObjE[I, Cardinality.One, E, A]

  type IObj1[I, A[_ <: Mode]] = IObjE[I, Cardinality.One, Nothing, A]

  /**
    * [[IObjE]] is similar to [[ObjE]] but allows for indexing or
    * parameterization of the field lookup
    *
    * @tparam I The index or "argument" to this field lookup
    * @tparam M [[Cardinality]] of the set of objects returned by this field
    *          lookup
    * @tparam A Api-schema of the returned "object" values
    */
  type IObjE[I, M <: Cardinality, E, A[_ <: Mode]]

  type IObj[I, M <: Cardinality, A[_ <: Mode]] = IObjE[I, M, Nothing, A]
}

object Mode {

  final class Fetcher[F[_]] extends Mode {
    type AtomE[E, A] = F[Either[E, A]]
    type IAtomE[I, E, A] = I => F[Either[E, A]]
    type ObjE[M <: Cardinality, E, Api[_ <: Mode]] =
      F[Either[E, M#Coll[maia.Fetcher[F, Api]]]]
    type IObjE[I, M <: Cardinality, E, Api[_ <: Mode]] =
      I => F[Either[E, M#Coll[maia.Fetcher[F, Api]]]]
  }

  final class Query[Super[_ <: Mode]] extends Mode {

    type AtomE[E, A] = Lookup[Super, E, A]
    type IAtomE[I, E, A] = I => Lookup[Super, E, A]

    trait ObjE[M <: Cardinality, E, Sub[_ <: Mode]] {
      def apply[R](cont: maia.Query[Sub] => Lookup[Sub, E, R])
        : Lookup[Super, E, M#Coll[R]]
    }

    trait IObjE[I, M <: Cardinality, E, Sub[_ <: Mode]] {
      def apply[R](ix: I)(cont: maia.Query[Sub] => Lookup[Sub, E, R])
        : Lookup[Super, E, M#Coll[R]]
    }
  }

  sealed trait Response extends maia.Mode {
    type AtomE[E, A] = Option[Either[E, A]]
    type IAtomE[I, E, A] = HashMap[I, Either[E, A]]
    type ObjE[M <: maia.Cardinality, E, A[_ <: maia.Mode]] =
      Option[Either[E, M#Coll[maia.Response[A]]]]
    type IObjE[I, M <: maia.Cardinality, E, A[_ <: maia.Mode]] =
      HashMap[I, Either[E, M#Coll[maia.Response[A]]]]
  }

  object Response extends Response

  sealed trait Request extends maia.Mode {
    type AtomE[E, A] = Boolean
    type IAtomE[I, E, A] = Set[I]
    type ObjE[M <: maia.Cardinality, E, A[_ <: maia.Mode]] =
      Option[maia.Request[A]]
    type IObjE[I, M <: maia.Cardinality, E, A[_ <: maia.Mode]] =
      HashMap[I, maia.Request[A]]
  }

  object Request extends Request

}
