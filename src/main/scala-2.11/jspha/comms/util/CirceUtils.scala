/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.util

private[comms] object CirceUtils {

  import io.circe._

  /**
    * A common pattern for constructing recursive Circe encoders. Trying to
    * construct these directly will lead to infinite loops either at
    * compile time or runtime depending on the method. By defining a
    * implicit lazy val or def which uses lazyEncoder internally you can
    * avoid creating the encoder too eagerly and avoid that infinite loop.
    * See also lazyDecoder.
    */
  def lazyEncoder[A](encoder: => Encoder[A]): Encoder[A] =
    new Encoder[A] {
      lazy val delegate: Encoder[A] = encoder
      def apply(a: A): Json = delegate(a)
    }

  /**
    * See lazyEncoder.
    */
  def lazyDecoder[A](decoder: => Decoder[A]): Decoder[A] =
    new Decoder[A] {
      lazy val delegate: Decoder[A] = decoder
      def apply(c: HCursor): Decoder.Result[A] = delegate(c)
    }

}
