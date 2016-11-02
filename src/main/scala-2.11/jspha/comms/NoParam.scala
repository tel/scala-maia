/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import io.circe.{KeyDecoder, KeyEncoder}

case class NoParam()

object NoParam {

  implicit val NoParamDefault: Default[NoParam] = new Default[NoParam] {
    val value: NoParam =
      NoParam()
  }

  implicit val NoParamKeyEncoder: KeyEncoder[NoParam] =
    new KeyEncoder[NoParam] {
      def apply(key: NoParam): String =
        ""
    }

  implicit val NoParamKeyDecoder: KeyDecoder[NoParam] =
    new KeyDecoder[NoParam] {
      def apply(key: String): Option[NoParam] =
        Some(NoParam())
    }
}
