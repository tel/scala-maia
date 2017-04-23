/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.serialization

import scala.language.higherKinds
import cats.Functor

final case class Section[P <: Section.Params, S](
  inject: S => P#InjectEff[P#Target],
  retract: P#Target => P#RetractEff[S]) {

  def map[SS](preSource: SS => S, postSource: S => SS)(
    implicit F: Functor[P#RetractEff]): Section[P, SS] =
    Section[P, SS](
      inject = ss => inject(preSource(ss)),
      retract = t => F.map(retract(t))(postSource)
    )

}

object Section {

  trait Params {
    type Target
    type InjectEff[A]
    type RetractEff[A]
  }

  object Params {
    type Aux[Target0, InjectEff0[_], RetractEff0[_]] = Params {
      type Target = Target0
      type InjectEff[A] = InjectEff0[A]
      type RetractEff[A] = RetractEff0[A]
    }
  }

}
