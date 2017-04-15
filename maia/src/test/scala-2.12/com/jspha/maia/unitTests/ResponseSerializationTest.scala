/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.unitTests

import com.jspha.maia.generic.{ResponseDecoder, ResponseEncoder}
import com.jspha.maia.{Cardinality, Fields, Response}
import io.circe._

import scala.collection.immutable.HashMap

final case class ResponseSerializationTest[F <: Fields](
  x: F#Atom[Int],
  y: F#AtomE[String, Int],
  q: F#IAtom[Int, Int],
  z: F#Atom[String],
  p: F#Obj[Cardinality.Opt, ResponseSerializationTest.Sibling],
  o: F#IObjE[Int, Cardinality.Many, String, ResponseSerializationTest.Sibling]
)

object ResponseSerializationTest {

  final case class Sibling[F <: Fields](
    x: F#Atom[Int],
    y: F#ObjE1[String, ResponseSerializationTest]
  )

  val respEncoder: Encoder[Response[ResponseSerializationTest]] =
    implicitly[ResponseEncoder[ResponseSerializationTest]]

  val respDecoder: Decoder[Response[ResponseSerializationTest]] =
    implicitly[ResponseDecoder[ResponseSerializationTest]]

  val sibling: Response[Sibling] =
    Sibling[Fields.Response](
      x = Some(Right(1)),
      y = None
    )

  val response: Response[ResponseSerializationTest] =
    ResponseSerializationTest[Fields.Response](
      x = Some(Right(3)),
      y = Some(Left("missing")),
      q = HashMap(2 -> Right(3)),
      z = None,
      p = Some(Right(Some(sibling))),
      o = HashMap(2 -> Right(List(sibling, sibling)), 3 -> Left("bad id"))
    )

  val json: Json =
    respEncoder(response)

  val decoded: Decoder.Result[Response[ResponseSerializationTest]] =
    respDecoder.decodeJson(json)

}
