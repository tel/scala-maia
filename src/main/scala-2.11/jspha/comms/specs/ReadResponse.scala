package jspha.comms.specs

import cats.Functor
import cats.data.Xor
import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.TypeTag
import scala.language.higherKinds
import jspha.comms._
import shapeless._
import shapeless.labelled._

import scala.collection.generic.CanBuildFrom

trait ReadResponse extends Qs {
  type Atomic[P, M <: Multiplicity, A] = Decoder[Response]
  type Nested[P, M <: Multiplicity, T[_ <: Qs]] = Decoder[Response]
}

object ReadResponse extends ReadResponse {

  type RR = ReadResponse

  trait Auto[T] {
    val decoder: Decoder[Response]
  }

  trait ReadObject[T] {
    import ReadObject._
    val value: Map[String, Slot]
  }

  object ReadObject {

    case class Slot(
        decodeKey: Decoder[Key.Dyn],
        decodeBody: Decoder[MSet[Dyn] Xor MSet[Response]]
    )

    implicit val ReadObjectHNil: ReadObject[HNil] =
      new ReadObject[HNil] {
        val value: Map[String, Slot] = Map()
      }

    implicit def ReadObjectAtomic[K <: Symbol,
                                  P: Encoder: Decoder: TypeTag,
                                  M <: Multiplicity,
                                  A: TypeTag,
                                  T <: HList](
        implicit kWitness: Witness.Aux[K],
        readObjectT: Lazy[ReadObject[T]],
        mSelector: Multiplicity.MSelector[M],
        mFunctor: Functor[M#Apply],
        decoderA: Decoder[M#Apply[A]]
    ): ReadObject[FieldType[K, RR#Atomic[P, M, A]] :: T] =
      new ReadObject[FieldType[K, RR#Atomic[P, M, A]] :: T] {
        val dynDecoder: Decoder[Xor[MSet[Dyn], MSet[Response]]] =
          decoderA.map { p =>
            Xor.left(mSelector.embed(mFunctor.map(p)(Dyn(_))))
          }
        val keyDecoder: Decoder[Key.Dyn] = Key.Dyn.dynamicKeyDecoder[P]
        val value: Map[String, Slot] =
          readObjectT.value.value +
            (kWitness.value.name -> Slot(keyDecoder, dynDecoder))
      }

    implicit def ReadObjectNested[K <: Symbol,
                                  P: Encoder: Decoder: TypeTag,
                                  M <: Multiplicity,
                                  A[_ <: Qs],
                                  T <: HList](
        implicit kWitness: Witness.Aux[K],
        readObjectT: Lazy[ReadObject[T]],
        mSelector: Multiplicity.MSelector[M],
        mFunctor: Functor[M#Apply],
        decoderA: Lazy[Auto[A[RR]]]
    ): ReadObject[FieldType[K, RR#Nested[P, M, A]] :: T] =
      new ReadObject[FieldType[K, RR#Nested[P, M, A]] :: T] {
        val subResponseDecoder: Decoder[Response] =
          decoderA.value.decoder
        val dynDecoder: Decoder[Xor[MSet[Dyn], MSet[Response]]] =
          mSelector.decoder[Response](subResponseDecoder).map { p =>
            Xor.right(mSelector.embed(p))
          }
        val keyDecoder: Decoder[Key.Dyn] = Key.Dyn.dynamicKeyDecoder[P]
        val value: Map[String, Slot] =
          readObjectT.value.value +
            (kWitness.value.name -> Slot(keyDecoder, dynDecoder))
      }

  }

}
