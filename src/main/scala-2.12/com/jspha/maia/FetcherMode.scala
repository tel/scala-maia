/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

class FetcherMode[M[_]] extends Mode {
  type Atom[A] = M[A]
  type Obj[Api[_ <: Mode]] = M[Fetcher[M, Api]]
}
