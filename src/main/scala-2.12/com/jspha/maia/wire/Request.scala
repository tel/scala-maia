/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.wire

import scala.collection.immutable.HashMap
import io.circe._, io.circe.generic.semiauto._

sealed trait Request {

  def ++(other: Request): Request = ???

}

object Request {
  case object Unit extends Request
  case object Atom extends Request
  case class Obj(mapping: HashMap[String, Request]) extends Request

  val unit: Request =
    Unit
  val atom: Request =
    Atom
  def obj(elems: (String, Request)*): Request =
    Obj(HashMap(elems: _*))

  implicit val RequestDecoder: Decoder[Request] = deriveDecoder[Request]
  implicit val RequestEncoder: Encoder[Request] = deriveEncoder[Request]

}
