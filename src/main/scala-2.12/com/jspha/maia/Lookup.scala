/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import cats._
import cats.data.Validated

final case class Lookup[Api[_ <: Mode], E, +R](
  request: Request[Api],
  handleResponse: Response[E, Api] => Validated[LookupError[E], R]
) {

  def handle: Lookup[Api, E, Validated[LookupError[E], R]] =
    Lookup[Api, E, Validated[LookupError[E], R]](
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
