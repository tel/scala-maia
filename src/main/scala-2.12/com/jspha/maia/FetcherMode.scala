/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

class FetcherMode[F[_]] extends Mode {
  type Atom[A] = F[A]
  type IndexedAtom[I, A] = I => F[A]
  type Obj[Api[_ <: Mode]] = F[Fetcher[F, Api]]
  type ObjM[M <: Multiplicity, Api[_ <: Mode]] = F[M#Wrap[Fetcher[F, Api]]]
  type IndexedObj[I, Api[_ <: Mode]] = I => F[Fetcher[F, Api]]
}
