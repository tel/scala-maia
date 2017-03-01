/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

// NOTE: We need to use Options here. It might feel like our types could
// prevent the need for Options here (and subsequently, certain kinds of
// failures later) but that would require type parameterization on
// information about particular Requests---that's (probably) a bridge too far

object ResponseMode extends Mode {
  type Atom[A] = Option[A]
  type Obj[A[_ <: Mode]] = Option[Response[A]]
}
