/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.wire

import scala.collection.immutable.HashMap
import io.circe._, io.circe.generic.semiauto._

sealed trait Response
object Response {
  case object Unit extends Response
  case class Atom(value: String) extends Response
  case class Obj(mapping: HashMap[String, Response]) extends Response

  val unit: Response =
    Unit
  def atom(value: String): Response =
    Atom(value)
  def obj(elems: (String, Response)*): Response =
    Obj(HashMap(elems: _*))

  // NOTE: Circe uses asInstanceOf in its macros. This is an acceptable risk.

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  implicit val ResponseDecoder: Decoder[Response] = deriveDecoder[Response]

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  implicit val ResponseEncoder: Encoder[Response] = deriveEncoder[Response]
}
