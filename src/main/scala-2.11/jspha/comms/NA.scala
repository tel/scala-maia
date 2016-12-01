/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import io.circe.{KeyDecoder, KeyEncoder}

/**
  * Na, not applicable, is a singleton type which indicates a default
  * argument for an Api specification.
  */
case class NA()

object NA {

  implicit val hasDefault: Default[NA] =
    Default(NA())

  implicit val hasKeyEncoder: KeyEncoder[NA] =
    new KeyEncoder[NA] {
      def apply(key: NA): String =
        ""
    }

  implicit val hasKeyDecoder: KeyDecoder[NA] =
    new KeyDecoder[NA] {
      def apply(key: String): Option[NA] =
        Some(NA())
    }
}
