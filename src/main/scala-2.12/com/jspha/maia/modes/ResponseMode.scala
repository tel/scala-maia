/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.modes

import com.jspha.maia.Mode
import jspha.maia.Mode

import scala.language.higherKinds
import jspha.maia.simple._

object ResponseMode extends Mode {
  type Atom[A] = Option[A]
  type Obj[A[_ <: Mode]] = Option[Response[A]]
}

