/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import cats._
import cats.data.Validated

final case class Lookup[Api[_ <: Mode], R](
  buildRequest: Request[Api],
  analyzeResponse: Response[Api] => Validated[LookupError, R]
) {

  def handle: Lookup[Api, Validated[LookupError, R]] =
    Lookup[Api, Validated[LookupError, R]](
      buildRequest,
      analyzeResponse = analyzeResponse andThen Validated.valid
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
          buildRequest = merger.apply(ff.buildRequest, fa.buildRequest),
          analyzeResponse = resp =>
            ValAp.ap(ff.analyzeResponse(resp))(fa.analyzeResponse(resp))
        )

      def map[A, B](fa: Lookup[Api, A])(f: A => B): Lookup[Api, B] =
        fa.copy(analyzeResponse = resp => fa.analyzeResponse(resp).map(f))
    }

}
