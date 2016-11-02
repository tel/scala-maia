/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.test

import cats.data.Xor
import io.circe.{Decoder, Encoder}
import jspha.comms._
import jspha.comms.qs._
import utest._

object Serialization extends TestSuite {

  val tests = this {

    'wire {
      'Request {
        'Atomic {
          val req = wire.Request.unit('foo, Set("bar", "baz", "quux"))
          val out = Encoder[wire.Request].apply(req)
          val in = Decoder[wire.Request].decodeJson(out)
          out.noSpaces ==> """{"foo":{"Atomic":["bar","baz","quux"]}}"""
          in ==> Xor.Right(req)
        }
      }
    }

    case class Non[Q <: Qs]()
    case class Test1[Q <: Qs](x: Q#Atomic1[Int])
    case class Test2[Q <: Qs](x: Q#Atomic1[Int], y: Q#Atomic1[Int])

    'Response {
      import shapeless._
      import shapeless.labelled._
      import jspha.comms.qs.RespS._
      import jspha.comms.qs.RespS.ResponseEncoder._

      val test1 = implicitly[ObjRespEncoder[HNil]]
      val test2 = implicitly[ResponseEncoder[Non]]

      val xWit = Witness('x)
      type K = xWit.T
      type P = NoParam
      type M = Mult.One
      type A = Int
      type V = RespS#Atomic[P, M, A]
      type T = HNil

      val test3 = implicitly[ObjRespEncoder[FieldType[K, V] :: T]]
//      val test4 = implicitly[ResponseEncoder[Test1]]

    }

  }

}
