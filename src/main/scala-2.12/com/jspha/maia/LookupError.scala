/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats.Semigroup

sealed trait LookupError

object LookupError {

  final case class Parallel(left: LookupError, right: LookupError)
      extends LookupError

  final case class Nested(groupName: String, subError: LookupError)
      extends LookupError

  // NOTE: This is marked CRITICAL because it ought to suggest a
  // library/server compliance error as opposed to an error
  // intentionally reported by the server.
  final case class ResponseMissingCRITICAL(key: String) extends LookupError

  implicit val LookupErrorIsSemiGroup: Semigroup[LookupError] =
    (x: LookupError, y: LookupError) => Parallel(x, y)

}
