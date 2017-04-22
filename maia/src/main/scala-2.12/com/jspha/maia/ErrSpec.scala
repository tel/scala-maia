/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

sealed trait ErrSpec {
  type Fold[IfHasErr[_] <: Up, IfNoErr <: Up, Up] <: Up
  type Provide[X]
  type ErrValue
}
sealed trait HasErr[E] extends ErrSpec {
  type Fold[IfHasErr[_] <: Up, IfNoErr <: Up, Up] = IfHasErr[E]
  type Provide[X] = Either[E, X]
  type ErrValue = E
}
sealed trait NoErr extends ErrSpec {
  type Fold[IfHasErr[_] <: Up, IfNoErr <: Up, Up] = IfNoErr
  type Provide[X] = X
  type ErrValue = Nothing
}
