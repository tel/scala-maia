/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.modes

import com.jspha.maia

import scala.language.higherKinds

class Fetcher[F[_]] extends maia.Mode {
  type Atom[A] = F[A]
  type IAtom[I, A] = I => F[A]
  type Obj[M <: maia.Cardinality, Api[_ <: maia.Mode]] =
    F[M#Coll[maia.Fetcher[F, Api]]]
  type IObj[I, M <: maia.Cardinality, Api[_ <: maia.Mode]] =
    I => F[M#Coll[maia.Fetcher[F, Api]]]
}
