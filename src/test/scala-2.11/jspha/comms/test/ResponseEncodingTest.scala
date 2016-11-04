/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.test

import utest._
import shapeless._
import shapeless.labelled._
import jspha.comms._
import scala.collection.immutable.HashMap

/**
  * NOTE: Response encodings are not actually part of a public interface but
  * instead are merely gold standard tests to ensure that the encoders are
  * being properly generated.
  */
object ResponseEncodingTest extends TestSuite {

  val tests = this {

    'Encoder {
      'Non {
        case class Non[S <: Spec]()
        val resp: Response[Non] = Non()
        resp.asJson.noSpaces ==> """{}"""
      }
      'SingleAtomic {
        case class Single[S <: Spec](x: S#Atomic[Na, Cardinality.Singular, Int])
        val resp: Response[Single] =
          Single[ResponseSpec](x = HashMap(Na() -> CSet.Singular(10)))
        resp.asJson.noSpaces ==> """{"x":{"":{"Singular":10}}}"""
      }
      'DoubleAtomic {
        case class Double[S <: Spec](
          x: S#Atomic[Na, Cardinality.Singular, Int],
          y: S#Atomic[Int, Cardinality.Variable, Int]
        )
        val resp: Response[Double] =
          Double[ResponseSpec](
            x = HashMap(Na() -> CSet.Singular(10)),
            y = HashMap(
              1 -> CSet.Variable(List(1, 2, 3)),
              2 -> CSet.Variable(List(4, 5, 6))
            )
          )
        val xRepr = """{"":{"Singular":10}}"""
        val yRepr = """{"1":{"Variable":[1,2,3]},"2":{"Variable":[4,5,6]}}"""
        resp.asJson.noSpaces ==> s"""{"x":$xRepr,"y":$yRepr}"""
      }
      'SingleNested {
        case class Inner[S <: Spec]()
        case class Outer[S <: Spec](
          inner: S#Nested[Na, Cardinality.Singular, Inner]
        )
        val respInner: Response[Inner] = Inner()
        val resp: Response[Outer] = Outer[ResponseSpec](
          inner = HashMap(Na() -> CSet.Singular(respInner))
        )
        resp.asJson.noSpaces ==> """{"inner":{"":{"Singular":{}}}}"""
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
