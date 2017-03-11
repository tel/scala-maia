/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

class FetcherMode[F[_]] extends Mode {
  type Atom[A] = F[A]
  type IAtom[I, A] = I => F[A]
  type Obj[M <: Cardinality, Api[_ <: Mode]] = F[M#Coll[Fetcher[F, Api]]]
  type IObj[I, M <: Cardinality, Api[_ <: Mode]] =
    I => F[M#Coll[Fetcher[F, Api]]]
}
