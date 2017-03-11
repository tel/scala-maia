/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.modes

import com.jspha.maia
import scala.language.higherKinds

class Query[Super[_ <: maia.Mode]] extends maia.Mode {

  type Atom[A] = maia.Lookup[Super, A]
  type IAtom[I, A] = I => maia.Lookup[Super, A]

  trait Obj[M <: maia.Cardinality, Sub[_ <: maia.Mode]] {
    def apply[R](cont: maia.Query[Sub] => maia.Lookup[Sub, R])
      : maia.Lookup[Super, M#Coll[R]]
  }

  trait IObj[I, M <: maia.Cardinality, Sub[_ <: maia.Mode]] {
    def apply[R](ix: I)(cont: maia.Query[Sub] => maia.Lookup[Sub, R])
      : maia.Lookup[Super, M#Coll[R]]
  }
}
