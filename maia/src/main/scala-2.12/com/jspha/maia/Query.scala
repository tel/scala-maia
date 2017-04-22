/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._
import cats.data.Validated
import cats.syntax.cartesian._
import com.jspha.maia.internal.ReqTree

import scala.language.higherKinds

final case class Query[T[_ <: Dsl], +E, +R](
  requests: ReqTree[T],
  handleResponse: Response[T] => Validated[Query.Error[E], R]
) {

  /**
    * Capture all errors at once and handle them locally. A failing
    * [[Query]] no longer fails one [[handle]] has been called: errors will
    * not be propagated upward.
    */
  def handle: Query[T, Nothing, Validated[Query.Error[E], R]] =
    Query[T, Nothing, Validated[Query.Error[E], R]](
      requests,
      handleResponse = resp => Validated.valid(handleResponse(resp))
    )

  def map[S](f: R => S): Query[T, E, S] =
    copy[T, E, S](handleResponse = resp => handleResponse(resp).map(f))

  def request(implicit merger: typelevel.MergeRequests[T]): Request[T] =
    requests.request(merger)

  // scalastyle:off

  /**
    * This is [[cats.syntax.CartesianOps.|@|]] under a different name.
    * Unfortunately, instance resolution doesn't seem to work (despite
    * [[Query.QueryIsCartesian]]), so using [[*]] drives inference manually
    * to the right result.
    */
  def *[EE >: E, RR >: R, S](other: Query[T, EE, S]) =
    catsSyntaxCartesian[Query[T, EE, ?], RR](this)(
      Query.QueryIsCartesian[T, EE]) |@| other

  // scalastyle:on

}

object Query {

  trait Transformer[S[_ <: Dsl], T[_ <: Dsl], E, C <: Size] {
    def apply[R](cont: QueriesAt[T] => Query[T, E, R]): Query[S, E, C#Coll[R]]
  }

  implicit def QueryIsFunctor[T[_ <: Dsl], E]: Functor[Query[T, E, ?]] =
    new Functor[Query[T, E, ?]] {
      def map[A, B](fa: Query[T, E, A])(f: A => B) =
        fa.copy[T, E, B](handleResponse = resp =>
          fa.handleResponse(resp).map(f))
    }

  implicit def QueryIsCartesian[T[_ <: Dsl], E]: Cartesian[Query[T, E, ?]] =
    new Cartesian[Query[T, E, ?]] {
      def product[A, B](fa: Query[T, E, A],
                        fb: Query[T, E, B]): Query[T, E, (A, B)] =
        Query[T, E, (A, B)](
          requests = ReqTree.Branch(fa.requests, fb.requests),
          handleResponse = resp =>
            (fa.handleResponse(resp) |@| fb.handleResponse(resp)).tupled
        )
    }

  sealed trait Error[+E] {

    /**
      * Transforms the [[Error]] to correspond to a different domain error
      * type.
      */
    def map[F](f: E => F): Error[F]
  }

  object Error {

    /**
      * Errors occur in [[Parallel]] when unhandled errors exist along two
      * parallel branches of a [[Query]]. These errors are collected all
      * together so that error reports are complete.
      */
    final case class Parallel[E](left: Error[E], right: Error[E])
        extends Error[E] {
      def map[F](f: (E) => F): Error[F] =
        copy(left = left.map(f), right = right.map(f))
    }

    /**
      * Having the [[Parallel]] error type enables [[Error]] to form a
      * [[Semigroup]] and, subsequently, collect errors with
      * [[cats.data.Validated]].
      */
    implicit def ErrorIsSemiGroup[E]: Semigroup[Error[E]] =
      (x: Error[E], y: Error[E]) => Parallel(x, y)

    /**
      * When a [[Error]] arises in an object lookup we collect those errors
      * and mark them with the object where they fail. In aggregate, this forms
      * a trie of errors.
      */
    final case class Object[E](key: Symbol, subError: Error[E])
        extends Error[E] {
      def map[F](f: (E) => F): Error[F] = copy(subError = subError.map(f))
    }

    /**
      * A [[Domain]] [[Error]] is an error returned by the server.
      */
    final case class Domain[E](err: E) extends Error[E] {
      def map[F](f: (E) => F): Error[F] = copy(err = f(err))
    }

    /**
      * Unexpected errors are those which arise out of a guarantee broken by
      * the library. End users are not expected to be able to handle them
      * necessarily. If one of these arises in your code, please leave an Issue
      * on the repo---it is a bug.
      */
    final case class Unexpected(err: UnexpectedError) extends Error[Nothing] {
      def map[F](f: (Nothing) => F): Error[F] = copy()
    }

    sealed trait UnexpectedError

    object UnexpectedError {

      /**
        * This error is reported when the the server did not report *any*
        * response to a requested field. Since (a) servers are guaranteed to
        * reply somehow to all requested fields and (b) [[Query]] values are
        * always constructed with requests that cover all response-fields
        * needed for handling... this error should never arise.
        */
      final case class ServerShouldHaveResponded(key: Symbol)
          extends UnexpectedError

    }

  }

}
