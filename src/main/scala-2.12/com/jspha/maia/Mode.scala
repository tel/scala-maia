/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

trait Mode {
  type Atom[A]
  type IndexedAtom[I, A]

  type Obj[A[_ <: Mode]]
  type IndexedObj[I, A[_ <: Mode]]
}
