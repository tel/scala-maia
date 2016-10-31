package jspha.comms.test.interpretation

import jspha.comms.Api
import jspha.comms.api.Presentation._
import jspha.comms.interpretation.Querying
import shapeless.labelled.FieldType
import shapeless.{::, HNil, Witness, the}
import shapeless.record._
import utest._

object SelectorTest extends TestSuite {

  val tests = this {

    'Manual {
      val xWitness =
        Witness('x)

      val HNilSel =
        the[Querying[HNil]]

      val HConsSel =
        the[Querying[FieldType[xWitness.T, Atomic1[Int]] :: HNil]]

      'HNil {
        * - { HNilSel.value ==> HNil }
        * - { HNilSel.value.keys ==> HNil }
      }
      'HCons {
        * - { HConsSel.value.record.x.name ==> "x" }
        * - { HConsSel.value.keys ==> 'x :: HNil }
      }
    }

    'Generic {

      case class T(x: Atomic1[Int], y: Atomic1[Char])
      val tSel = the[Querying[T]]

      * - { tSel.value.record.x.name ==> "x" }
      * - { tSel.value.record.y.name ==> "y" }

    }

    'Recursive {

      case class Loc(
        lat: Atomic1[Double],
        lon: Atomic1[Double]
      ) extends Api
      case class City(
        name: Atomic1[String],
        loc: Nested1[Loc],
        major: Nested1[User]
      ) extends Api
      case class User(
        home: Nested1[City],
        loc: Nested1[Loc],
        name: Atomic1[String]
      ) extends Api
      case class Query(
        currentUser: Nested1[User]
      ) extends Api

      val q0 = the[Querying[Query]].natural

    }

  }

}

