/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.props

import scala.language.higherKinds
import jspha.maia.simple._

// TODO: Implement this computation via implicits
trait MergeRequests[Api[_ <: Mode]] {
  def merge(a: Request[Api], b: Request[Api]): Request[Api]
}
