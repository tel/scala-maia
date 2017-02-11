/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds
import cats._

case class Lookup[Api[_ <: Mode], R](
  print: Request[Api],
  parse: Response[Api] => R
)

object Lookup {

  implicit def LookupIsApply[Api[_ <: Mode]](
    implicit merger: props.MergeRequests[Api]
  ): Apply[Lookup[Api, ?]] =
    new Apply[Lookup[Api, ?]] {
      def ap[A, B](ff: Lookup[Api, A => B])(fa: Lookup[Api, A]) =
        Lookup[Api, B](
          print = merger.merge(ff.print, fa.print),
          parse = resp => ff.parse(resp)(fa.parse(resp))
        )

      def map[A, B](fa: Lookup[Api, A])(f: A => B): Lookup[Api, B] =
        fa.copy(parse = fa.parse andThen f)
    }

}
