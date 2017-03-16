/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.unitTests

import com.jspha.maia.generic.{RequestDecoder, RequestEncoder}
import com.jspha.maia.{Cardinality, Fields, Request}
import io.circe._

import scala.collection.immutable.HashMap

final case class RequestSerializationTest[F <: Fields](
  x: F#Atom[Int],
  y: F#AtomE[String, Int],
  z: F#IAtomE[Int, String, Int],
  w: F#Obj[Cardinality.Many, RequestSerializationTest.Sibling],
  v: F#IObj[Int, Cardinality.Many, RequestSerializationTest.Sibling]
)

object RequestSerializationTest {

  final case class Sibling[F <: Fields](
    encoderTest: F#ObjE[Cardinality.Many, String, RequestSerializationTest]
  )

  val reqEncoder: Encoder[Request[RequestSerializationTest]] =
    implicitly[RequestEncoder[RequestSerializationTest]]

  val reqDecoder: Decoder[Request[RequestSerializationTest]] =
    implicitly[RequestDecoder[RequestSerializationTest]]

  val siblingRequest: Request[Sibling] =
    Sibling[Fields.Request](
      encoderTest = Some(
        RequestSerializationTest[Fields.Request](
          x = true,
          y = false,
          z = Set(3),
          w = None,
          v = HashMap()
        ))
    )

  val request: Request[RequestSerializationTest] =
    RequestSerializationTest[Fields.Request](
      x = true,
      y = false,
      z = Set(3),
      w = Some(siblingRequest),
      v = HashMap(
        3 -> siblingRequest
      )
    )

  val json: Json = reqEncoder(request)

  val decoded: Decoder.Result[Request[RequestSerializationTest]] =
    reqDecoder.decodeJson(json)

}
