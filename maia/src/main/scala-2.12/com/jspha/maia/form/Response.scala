/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.form

import scala.language.higherKinds
import com.jspha.maia
import com.jspha.maia._

sealed trait Response extends Dsl {

  type AtomK[A, As <: ArgSpec, Es <: ErrSpec, C <: Size] =
    As#Provide[Es#Provide[C#Coll[A]]]
  type ObjK[T[_ <: Dsl], As <: ArgSpec, Es <: ErrSpec, C <: Size] =
    As#Provide[Es#Provide[C#Coll[maia.Response[T]]]]

}

object Response extends Response
