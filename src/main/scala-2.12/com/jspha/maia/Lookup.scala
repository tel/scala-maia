/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import cats._
import cats.syntax.CartesianOps
import cats.data.Validated

/**
  * A value representing a request against an Api and its eventual
  * interpretation as a result or error.
  *
  * Lookups are [[Apply]] and thus can be combined to build more
  * sophisticated results from server responses.
  *
  * @param request A [[Request]] for the given `Api` that represents this
  *                [[Lookup]]
  * @param handleResponse A process of converting a [[Response]] from this
  *                       `Api` into either some [[LookupError]]s or a
  *                       response value of type `R`
  * @tparam Api The Api type this lookup is built from
  * @tparam E The type of errors which might be returned by the server when
  *           this Lookup is run
  * @tparam R The type of result produced by the client from the requested
  *           information
  */
final case class Lookup[Api[_ <: Mode], +E, +R] private[maia] (
  request: Request[Api],
  handleResponse: Response[Api] => Validated[LookupError[E], R]
) {

  /**
    * Capture all errors at once and handle them locally. A failing
    * [[Lookup]] no longer fails one [[handle]] has been called: errors will
    * not be propagated upward.
    */
  def handle: Lookup[Api, Nothing, Validated[LookupError[E], R]] =
    Lookup[Api, Nothing, Validated[LookupError[E], R]](
      request,
      handleResponse = resp => Validated.valid(handleResponse(resp))
    )

}

object Lookup {

  private def ValAp[E]: Apply[Validated[LookupError[E], ?]] =
    implicitly[Apply[Validated[LookupError[E], ?]]]

  implicit def LookupIsApply[Api[_ <: Mode], E](
    implicit merger: generic.MergeRequests[Api]
  ): Apply[Lookup[Api, E, ?]] =
    new Apply[Lookup[Api, E, ?]] {

      def ap[A, B](ff: Lookup[Api, E, A => B])(fa: Lookup[Api, E, A]) =
        Lookup[Api, E, B](
          request = merger.apply(ff.request, fa.request),
          handleResponse =
            resp => ValAp.ap(ff.handleResponse(resp))(fa.handleResponse(resp))
        )

      def map[A, B](fa: Lookup[Api, E, A])(f: A => B): Lookup[Api, E, B] =
        fa.copy[Api, E, B](handleResponse = resp =>
          fa.handleResponse(resp).map(f))
    }

}
