/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.collection.immutable.HashMap
import scala.language.higherKinds

sealed trait ArgSpec {
  type Fold[IfHasArg[_] <: Up, IfNoArg <: Up, Up] <: Up
  type Request[X]
  type Provide[X]
  type Terminal
}

sealed trait HasArg[A] extends ArgSpec {
  type Fold[IfHasArg[_] <: Up, IfNoArg <: Up, Up] = IfHasArg[A]
  type Request[X] = A => X
  type Provide[X] = HashMap[A, X]
  type Terminal = Set[A]
}

sealed trait NoArg extends ArgSpec {
  type Fold[IfHasArg[_] <: Up, IfNoArg <: Up, Up] = IfNoArg
  type Request[X] = X
  type Provide[X] = Option[X]
  type Terminal = Boolean
}
