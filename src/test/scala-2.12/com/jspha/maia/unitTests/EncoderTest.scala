/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.unitTests

import com.jspha.maia.generic.{RequestDecoder, RequestEncoder}
import com.jspha.maia.{Cardinality, Fields, Request}
import io.circe._

import scala.collection.immutable.HashMap

final case class EncoderTest[F <: Fields](
  x: F#Atom[Int],
  y: F#AtomE[String, Int],
  z: F#IAtomE[Int, String, Int],
  w: F#Obj[Cardinality.Many, EncoderTest.Sibling],
  v: F#IObj[Int, Cardinality.Many, EncoderTest.Sibling]
)

object EncoderTest {

  final case class Sibling[F <: Fields](
    encoderTest: F#ObjE[Cardinality.Many, String, EncoderTest]
  )

  val encoder: Encoder[Request[EncoderTest]] =
    implicitly[RequestEncoder[EncoderTest]]

  val decoder: Decoder[Request[EncoderTest]] =
    implicitly[RequestDecoder[EncoderTest]]

  val siblingRequest: Request[Sibling] =
    Sibling[Fields.Request](
      encoderTest = Some(
        EncoderTest[Fields.Request](
          x = true,
          y = false,
          z = Set(3),
          w = None,
          v = HashMap()
        ))
    )

  val request: Request[EncoderTest] =
    EncoderTest[Fields.Request](
      x = true,
      y = false,
      z = Set(3),
      w = Some(siblingRequest),
      v = HashMap(
        3 -> siblingRequest
      )
    )

  val json: Json = encoder(request)

  val decoded: Decoder.Result[Request[EncoderTest]] =
    decoder.decodeJson(json)

}
