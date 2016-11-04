/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.test

import utest._
import shapeless._
import shapeless.labelled._
import jspha.comms._

import scala.collection.immutable.HashMap

object ResponseEncodingTest extends TestSuite {

  val tests = this {

    'Encoder {
      'Non {
        case class Non[S <: Spec]()
        val nonResp: Response[Non] = Non()
        nonResp.asJson.noSpaces ==> """{}"""
      }
      'SingleAtomic {
        case class Single[S <: Spec](x: S#Atomic[Na, Cardinality.Singular, Int])
        val singleResp: Response[Single] =
          Single[ResponseSpec](x = HashMap(Na() -> CSet.Singular(10)))
        singleResp.asJson.noSpaces ==> """{"x":{"":{"Singular":10}}}"""
      }
    }

    'ObjectEncoder {

      def oe[T <: HList](t: T)(
          implicit enc: responseAux.ObjectEncoder[T]): String =
        enc(t).noSpaces

      'HNil { oe[HNil](HNil) ==> """{}""" }
      'HCons {
        val xWit = Witness('x)
        type K = xWit.T
        type P = Na
        type C = Cardinality.Singular
        type A = Int
        type T = HNil
        type V = ResponseSpec#Atomic[P, C, A]
        val tail: T = HNil
        val value: V = HashMap(Na() -> CSet.Singular(10))
        val atomic: FieldType[K, V] :: T = field[K](value) :: tail

        oe(atomic) ==> """{"x":{"":{"Singular":10}}}"""
      }

    }

  }

}
