/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.util

import com.jspha.maia.serialization._
import io.circe._

object CirceSerialization {

  sealed trait Params extends Section.Params {
    type Target = Json
    type InjectEff[A] = A
    type RetractEff[A] = Either[DecodingFailure, A]
  }

  def circeSection[A](implicit encoder: Encoder[A],
                      decoder: Decoder[A]): Section[Params, A] =
    Section[Params, A](
      inject = encoder.apply,
      retract = decoder.decodeJson
    )

}
