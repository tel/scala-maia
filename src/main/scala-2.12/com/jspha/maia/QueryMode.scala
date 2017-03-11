/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.language.higherKinds

class QueryMode[Super[_ <: Mode]] extends Mode {

  type Atom[A] = Lookup[Super, A]
  type IndexedAtom[I, A] = I => Lookup[Super, A]

  trait MultiObj[M <: Multiplicity, Sub[_ <: Mode]] {
    def apply[R](cont: Query[Sub] => Lookup[Sub, R]): Lookup[Super, M#Coll[R]]
  }

  trait IndexedObj[I, Sub[_ <: Mode]] {
    def apply[R](ix: I)(cont: Query[Sub] => Lookup[Sub, R]): Lookup[Super, R]
  }

  trait IndexedMultiObj[I, M <: Multiplicity, Sub[_ <: Mode]] {
    def apply[R](ix: I)(
      cont: Query[Sub] => Lookup[Sub, R]): Lookup[Super, M#Coll[R]]
  }
}
