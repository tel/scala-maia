/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.modes

import scala.language.higherKinds
import com.jspha.maia._

object RequestMode extends Mode {
  type Atom[A] = Boolean
  type Obj[A[_ <: Mode]] = Option[Request[A]]
}
