/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats.Semigroup

sealed trait LookupError

object LookupError {

  final case class Parallel(left: LookupError, right: LookupError)
      extends LookupError

  final case class Nested(key: Symbol, subError: LookupError)
      extends LookupError

  /***
    * Unexpected errors are those which arise out of a guarantee broken by
    * the library. End users are not expected to be able to handle them
    * necessarily. If one of these arises in your code, please leave an Issue
    * on the repo---it is a bug.
    */
  final case class Unexpected(err: UnexpectedError) extends LookupError

  implicit val LookupErrorIsSemiGroup: Semigroup[LookupError] =
    (x: LookupError, y: LookupError) => Parallel(x, y)

  sealed trait UnexpectedError

  object UnexpectedError {

    /***
      * This error is reported when the the server did not report *any*
      * response to a requested field. Since (a) servers are guaranteed to
      * reply somehow to all requested fields and (b) Lookup values are always
      * constructed with requests that cover all response-fields needed for
      * handling... this error should never arise.
      */
    final case class ServerShouldHaveResponded(key: Symbol)
        extends UnexpectedError

  }

}
