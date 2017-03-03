/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import cats._
import cats.data.Validated

final case class Lookup[Api[_ <: Mode], R](
  request: Request[Api],
  handleResponse: Response[Api] => Validated[LookupError, R]
) {

  def handle: Lookup[Api, Validated[LookupError, R]] =
    Lookup[Api, Validated[LookupError, R]](
      request,
      handleResponse = handleResponse andThen Validated.valid
    )

}

object Lookup {

  private val ValAp: Apply[Validated[LookupError, ?]] =
    implicitly[Apply[Validated[LookupError, ?]]]

  implicit def LookupIsApply[Api[_ <: Mode]](
    implicit merger: props.MergeRequests[Api]
  ): Apply[Lookup[Api, ?]] =
    new Apply[Lookup[Api, ?]] {
      def ap[A, B](ff: Lookup[Api, A => B])(fa: Lookup[Api, A]) =
        Lookup[Api, B](
          request = merger.apply(ff.request, fa.request),
          handleResponse =
            resp => ValAp.ap(ff.handleResponse(resp))(fa.handleResponse(resp))
        )

      def map[A, B](fa: Lookup[Api, A])(f: A => B): Lookup[Api, B] =
        fa.copy(handleResponse = resp => fa.handleResponse(resp).map(f))
    }

}
