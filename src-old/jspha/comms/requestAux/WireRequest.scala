/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.requestAux

import cats.Monoid
import cats.data.Xor
import io.circe._
import jspha.comms.util.CirceUtils

import scala.collection.immutable.HashMap

// NOTE: We use HashMaps here so that we can use the .merged method
case class WireRequest(
    here: HashMap[Symbol, Set[String] Xor HashMap[String, WireRequest]]
) {

  /**
    * Recursively merge requests so that all the query paths in each
    * component request are reflected in their combination. Does the best it
    * can do to resolve a disagreement about whether a given key is atomic or
    * nested (assumes nested with empty subrequests), but this is clearly a
    * library error.
    */
  def ++(other: WireRequest): WireRequest =
    WireRequest(here.merged(other.here) {
      case ((n1, Xor.Left(m1)), (n2, Xor.Right(m2))) =>
        (n1, Xor.Right(m1.foldLeft(m2) {
          case (map, key) => map + (key -> WireRequest())
        }))
      case ((n1, Xor.Right(m1)), (n2, Xor.Left(m2))) =>
        (n1, Xor.Right(m2.foldLeft(m1) {
          case (map, key) => map + (key -> WireRequest())
        }))
      case ((n1, Xor.Left(m1)), (n2, Xor.Left(m2))) =>
        (n1, Xor.Left(m1 ++ m2))
      case ((n1, Xor.Right(m1)), (n2, Xor.Right(m2))) =>
        (n1, Xor.Right(m1.merged(m2) {
          case ((p1, r1), (p2, r2)) => (p1, r1 ++ r2)
        }))
    })

}

object WireRequest {

  type Value = Set[String] Xor HashMap[String, WireRequest]

  def apply(pairs: (Symbol, Value)*): WireRequest =
    WireRequest(HashMap(pairs: _*))

  val zero: WireRequest =
    WireRequest()

  def unit[P: KeyEncoder](name: Symbol, keys: Set[P]): WireRequest = {
    val localSet: Value = Xor.left(keys.map(KeyEncoder[P].apply))
    WireRequest(name -> localSet)
  }

  def unit[P: KeyEncoder](name: Symbol, requests: HashMap[P, WireRequest]) = {
    val localRequests: HashMap[String, WireRequest] =
      requests.map(pair => (KeyEncoder[P].apply(pair._1), pair._2))
    val localXor: Value = Xor.right(localRequests)
    WireRequest(name -> localXor)
  }

  implicit val isMonoid: Monoid[WireRequest] = new Monoid[WireRequest] {
    def empty: WireRequest = zero
    def combine(x: WireRequest, y: WireRequest): WireRequest = x ++ y
  }

  implicit lazy val hasEncoder: Encoder[WireRequest] =
    CirceUtils.lazyEncoder {
      val hashMapEnc: Encoder[HashMap[String, WireRequest]] =
        Encoder.encodeMapLike(implicitly, hasEncoder)
      val xorEnc: Encoder[Value] =
        Encoder.encodeXor("Atomic", "Nested")(implicitly, hashMapEnc)
      val mapEnc: Encoder[HashMap[Symbol, Value]] =
        Encoder.encodeMapLike(implicitly, xorEnc)
      mapEnc.contramap(_.here)
    }

  implicit lazy val hasDecoder: Decoder[WireRequest] =
    CirceUtils.lazyDecoder {
      val hashMapDec: Decoder[HashMap[String, WireRequest]] =
        Decoder.decodeMapLike(implicitly, hasDecoder, implicitly)
      val xorDec: Decoder[Value] =
        Decoder.decodeXor("Atomic", "Nested")(implicitly, hashMapDec)
      val mapDec: Decoder[HashMap[Symbol, Value]] =
        Decoder.decodeMapLike(implicitly, xorDec, implicitly)
      mapDec.map(WireRequest(_))
    }

}
