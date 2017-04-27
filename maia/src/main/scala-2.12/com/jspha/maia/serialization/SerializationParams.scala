/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.serialization

import scala.language.higherKinds

trait SerializationParams {
  type Target
  type InjectEff[A]
  type RetractEff[A]
}

object SerializationParams {
  type Aux[Target0, InjectEff0[_], RetractEff0[_]] = SerializationParams {
    type Target = Target0
    type InjectEff[A] = InjectEff0[A]
    type RetractEff[A] = RetractEff0[A]
  }
}
