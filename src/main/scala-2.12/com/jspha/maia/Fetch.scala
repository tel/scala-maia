/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import cats._

case class Fetch[Api[_ <: Mode], R](
  print: Request[Api],
  parse: Response[Api] => R
)

object Fetch {

  implicit def LookupIsApply[Api[_ <: Mode]](
    implicit merger: props.MergeRequests[Api]
  ): Apply[Fetch[Api, ?]] =
    new Apply[Fetch[Api, ?]] {
      def ap[A, B](ff: Fetch[Api, A => B])(fa: Fetch[Api, A]) =
        Fetch[Api, B](
          print = merger.merge(ff.print, fa.print),
          parse = resp => ff.parse(resp)(fa.parse(resp))
        )

      def map[A, B](fa: Fetch[Api, A])(f: A => B): Fetch[Api, B] =
        fa.copy(parse = fa.parse andThen f)
    }

}

