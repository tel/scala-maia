package jspha.comms.specs

import cats.{Functor, Monad}
import cats.data.Xor
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

import scala.reflect.runtime.universe.TypeTag
import scala.language.higherKinds
import jspha.comms._
import shapeless._
import shapeless.labelled._

trait ReadResponse extends Qs {
  type Atomic[P, M <: Multiplicity, A] = Decoder[Response]
  type Nested[P, M <: Multiplicity, T[_ <: Qs]] = Decoder[Response]
}

object ReadResponse extends ReadResponse {

  type RR = ReadResponse

  trait Auto[T] {
    import Auto._
    import cats.syntax.foldable._
    import cats.instances.list._

    val slots: Map[String, Slot]

    private val basicKeyDecoder: Decoder[(String, Json)] =
      Decoder.decodeTuple2[String, Json]

    lazy val decoder: Decoder[Response] =
      Decoder.decodeCanBuildFrom[(Json, Json), List].emap { pairs =>
        pairs
          .foldM[Decoder.Result, Response](Response.zero) {
            case (resp, (keyJson, bodyJson)) =>
              for {
                (name, _) <- basicKeyDecoder.decodeJson(keyJson)
                Slot(keyDecoder, bodyDecoder) <- slots.get(name) match {
                  case None =>
                    Xor.left(
                      DecodingFailure(s"unexpected key named ${name}", List()))
                  case Some(s) =>
                    Xor.right(s)
                }
                keyRes <- keyDecoder.decodeJson(keyJson)
                bodyRes <- bodyDecoder.decodeJson(bodyJson)
              } yield resp + (keyRes -> bodyRes)
          }
          .leftMap(_.message)
      }

  }

  object Auto {

    case class Slot(
        decodeKey: Decoder[Key.Dyn],
        decodeBody: Decoder[MSet[Dyn] Xor MSet[Response]]
    )

    implicit def AutoGeneric[T, Repr <: HList](
        implicit gen: LabelledGeneric.Aux[T, Repr],
        ro: Auto[Repr]
    ): Auto[T] = ro.asInstanceOf

    implicit val AutoHNil: Auto[HNil] =
      new Auto[HNil] {
        val slots: Map[String, Slot] = Map()
      }

    implicit def AutoAtomic[K <: Symbol,
                            P: Encoder: Decoder: TypeTag,
                            M <: Multiplicity,
                            A: TypeTag,
                            T <: HList](
        implicit kWitness: Witness.Aux[K],
        readObjectT: Lazy[Auto[T]],
        mSelector: Multiplicity.MSelector[M],
        mFunctor: Functor[M#Apply],
        decoderA: Decoder[M#Apply[A]]
    ): Auto[FieldType[K, RR#Atomic[P, M, A]] :: T] =
      new Auto[FieldType[K, RR#Atomic[P, M, A]] :: T] {
        val dynDecoder: Decoder[Xor[MSet[Dyn], MSet[Response]]] =
          decoderA.map { p =>
            Xor.left(mSelector.embed(mFunctor.map(p)(Dyn(_))))
          }
        val keyDecoder: Decoder[Key.Dyn] = Key.Dyn.dynamicKeyDecoder[P]
        val slots: Map[String, Slot] =
          readObjectT.value.slots +
            (kWitness.value.name -> Slot(keyDecoder, dynDecoder))
      }

    implicit def AutoNested[K <: Symbol,
                            P: Encoder: Decoder: TypeTag,
                            M <: Multiplicity,
                            A[_ <: Qs],
                            T <: HList](
        implicit kWitness: Witness.Aux[K],
        readObjectT: Lazy[Auto[T]],
        mSelector: Multiplicity.MSelector[M],
        mFunctor: Functor[M#Apply],
        decoderA: Lazy[Auto[A[RR]]]
    ): Auto[FieldType[K, RR#Nested[P, M, A]] :: T] =
      new Auto[FieldType[K, RR#Nested[P, M, A]] :: T] {
        val subResponseDecoder: Decoder[Response] =
          decoderA.value.decoder
        val dynDecoder: Decoder[Xor[MSet[Dyn], MSet[Response]]] =
          mSelector.decoder[Response](subResponseDecoder).map { p =>
            Xor.right(mSelector.embed(p))
          }
        val keyDecoder: Decoder[Key.Dyn] = Key.Dyn.dynamicKeyDecoder[P]
        val slots: Map[String, Slot] =
          readObjectT.value.slots +
            (kWitness.value.name -> Slot(keyDecoder, dynDecoder))
      }

  }

}
