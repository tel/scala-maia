package jspha.comms.test.wire

import cats.data.Xor
import utest._
import jspha.comms.wire.{Key, Request}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

object RequestTest extends TestSuite {

  object up {
    def writeJs[A: Encoder](a: A): Json = Encoder[A].apply(a)
    def write[A: Encoder](a: A): String = Encoder[A].apply(a).noSpaces
    def readJs[A: Decoder](j: Json): DecodingFailure Xor A =
      Decoder[A].decodeJson(j)
  }

  val tests = this {

    'wire {

      val k1 = Key("name", up.writeJs[Int](3))
      val k2 = Key("name", up.writeJs[Boolean](false))
      val k3 = Key("name")

      val r0 = Request()
      val r1 = Request(k1 -> Request.empty)
      val r2 = Request(k2 -> r1)
      val r3 = Request(k3 -> r2, k2 -> r1)

      'ex0 { up.write(r0) ==> """[]""" }
      'ex1 { up.write(r1) ==> """[[["name",3],[]]]""" }
      'ex2 { up.write(r2) ==> """[[["name",false],[[["name",3],[]]]]]""" }

      def rtOk[A: Encoder: Decoder](a: A): Unit =
        up.readJs[A](up.writeJs(a)) ==> Xor.Right(a)

      'roundtrips {
        'ex1 { rtOk(r1) }
        'ex2 { rtOk(r2) }
        'ex3 { rtOk(r3) }
      }

      'Key {

        'ex1 { up.write(k1) ==> """["name",3]""" }
        'ex2 { up.write(k2) ==> """["name",false]""" }
        'ex3 { up.write(k3) ==> """["name",{}]""" }

        'roundtrips {

          'ex1 { up.readJs[Key](up.writeJs(k1)) ==> k1 }
          'ex2 { up.readJs[Key](up.writeJs(k2)) ==> k2 }
          'ex3 { up.readJs[Key](up.writeJs(k3)) ==> k3 }

        }

      }

    }

  }

}
