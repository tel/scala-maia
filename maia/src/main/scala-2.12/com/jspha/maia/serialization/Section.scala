/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.serialization

import scala.language.higherKinds
import cats.Functor

final case class Section[P <: SerializationParams, S](
  inject: S => P#InjectEff[P#Target],
  retract: P#Target => P#RetractEff[S]) {

  def map[SS](preSource: SS => S, postSource: S => SS)(
    implicit F: Functor[P#RetractEff]): Section[P, SS] =
    Section[P, SS](
      inject = ss => inject(preSource(ss)),
      retract = t => F.map(retract(t))(postSource)
    )

}
