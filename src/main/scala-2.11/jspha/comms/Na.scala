/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import io.circe.{KeyDecoder, KeyEncoder}

/**
  * Na, not applicable, is a singleton type which indicates a default
  * argument for an Api specification.
  */
case class Na()

object Na {

  implicit val NoParamDefault: Default[Na] =
    Default(Na())

  implicit val NoParamKeyEncoder: KeyEncoder[Na] =
    new KeyEncoder[Na] {
      def apply(key: Na): String =
        ""
    }

  implicit val NoParamKeyDecoder: KeyDecoder[Na] =
    new KeyDecoder[Na] {
      def apply(key: String): Option[Na] =
        Some(Na())
    }
}
