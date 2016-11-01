package jspha.comms.test

import cats.data.Xor
import io.circe.{Decoder, Encoder}
import jspha.comms.qs.wire.Request
import utest._

object Serialization extends TestSuite {

  val tests = this {

    'wire {
      'Request {
        'Atomic {
          val req = Request.unit('foo, Set("bar", "baz", "quux"))
          val out = Encoder[Request].apply(req)
          val in = Decoder[Request].decodeJson(out)
          out.noSpaces ==> """{"foo":{"Atomic":["bar","baz","quux"]}}"""
          in ==> Xor.Right(req)
        }
      }
    }

  }

}
