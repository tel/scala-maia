package jspha.comms.specs

import cats.data.Xor
import io.circe.Decoder

import scala.language.higherKinds
import jspha.comms._
import shapeless._
import shapeless.labelled._

trait ReadResponse extends Qs {
  type Atomic[P, M <: Multiplicity, A] = Decoder[Response]
  type Nested[P, M <: Multiplicity, T[_ <: Qs]] = Decoder[Response]
}

//object ReadResponse extends ReadResponse {
//
//  trait ResponseDecoder[T] {
//    val value: Map[String,
//                   (Decoder[Key], Decoder[MSet[Dyn] Xor MSet[Response]])]
//  }
//
//  object ResponseDecoder {
//
//    type Value = (Decoder[Key], Decoder[Xor[MSet[Dyn], MSet[Response]]])
//
//    implicit val ResponseDecoderHNil: ResponseDecoder[HNil] =
//      new ResponseDecoder[HNil] {
//        val value: Map[String, Value] = Map()
//      }
//
//    implicit def ResponseDecoderAtomic[P,
//                                       M <: Multiplicity,
//                                       A,
//                                       K <: Symbol,
//                                       T <: HList](
//        implicit kWitness: Witness.Aux[K],
//        tResponseDecoder: Lazy[ResponseDecoder[T]],
//        pDecoder: Decoder[P],
//        aDecoder: Decoder[A]
//    ): ResponseDecoder[FieldType[K, ReadResponse#Atomic[P, M, A]] :: T] =
//      new ResponseDecoder[FieldType[K, ReadResponse#Atomic[P, M, A]] :: T] {
//        val value: Map[String, Value] =
//          tResponseDecoder.value.value + (
//            kWitness.value.name -> (pDecoder, ???)
//          )
//      }
//
//  }
//
//}
