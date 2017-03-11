/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

trait Mode {
  type Atom[A]
  type IAtom[I, A]

  type Obj1[A[_ <: Mode]] = Obj[Cardinality.One, A]
  type Obj[M <: Cardinality, A[_ <: Mode]]

  type IObj1[I, A[_ <: Mode]] = IObj[I, Cardinality.One, A]
  type IObj[I, M <: Cardinality, A[_ <: Mode]]
}

object Mode {

  type Fetcher[F[_]] = modes.Fetcher[F]
  type Query[S[_ <: Mode]] = modes.Query[S]
  type Request = modes.Request
  type Response = modes.Response

}
