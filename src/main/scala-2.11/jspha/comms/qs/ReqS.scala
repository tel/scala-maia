package jspha.comms.qs

import cats.data.Xor
import io.circe.KeyEncoder

import scala.language.higherKinds
import scala.collection.immutable.HashMap
import jspha.comms._

trait ReqS extends Qs {
  type Atomic[P, M <: Mult, T] = Set[P]
  type Nested[P, M <: Mult, T[_ <: Qs]] = HashMap[P, Request[T]]
}

object ReqS {

  def toWire[Api[_ <: Qs]: ToWire](req: Request[Api]): wire.Request =
    ToWire[Api].toWire(req)

  trait ToWire[Api[_ <: Qs]] {
    def toWire(req: Request[Api]): wire.Request
  }

  object ToWire {

    import shapeless._
    import shapeless.labelled._

    def apply[Api[_ <: Qs]](implicit D: ToWire[Api]): ToWire[Api] = D

    implicit def GenericToWire[Api[_ <: Qs], Repr <: HList](
      implicit gen: LabelledGeneric.Aux[Request[Api], Repr],
      autoRepr: Lazy[Auto[Repr]]
    ): ToWire[Api] = new ToWire[Api] {
      def toWire(req: Request[Api]): wire.Request =
        autoRepr.value.toWire(gen.to(req))
    }

    trait Auto[T <: HList] {
      def toWire(t: T): wire.Request
    }

    implicit val AutoHNil: Auto[HNil] = new Auto[HNil] {
      def toWire(t: HNil): wire.Request = wire.Request.zero
    }

    implicit def AutoHConsAtomic[K <: Symbol, P, M <: Mult, A, T <: HList](
        implicit autoT: Lazy[Auto[T]],
        kWitness: Witness.Aux[K],
        pKeyEncoder: KeyEncoder[P]
    ): Auto[FieldType[K, ReqS#Atomic[P, M, A]] :: T] =
      new Auto[FieldType[K, ReqS#Atomic[P, M, A]] :: T] {
        def toWire(t: FieldType[K, ReqS#Atomic[P, M, A]] :: T): wire.Request = {
          autoT.value.toWire(t.tail) ++
            wire.Request.unit(kWitness.value, t.head)
        }
      }

    implicit def AutoHConsNested[K <: Symbol,
                                 P,
                                 M <: Mult,
                                 A[_ <: Qs],
                                 T <: HList](
        implicit autoT: Lazy[Auto[T]],
        kWitness: Witness.Aux[K],
        pKeyEncoder: KeyEncoder[P],
        toWireA: Lazy[ToWire[A]]
    ): Auto[FieldType[K, ReqS#Nested[P, M, A]] :: T] =
      new Auto[FieldType[K, ReqS#Nested[P, M, A]] :: T] {
        def toWire(t: FieldType[K, ReqS#Nested[P, M, A]] :: T): wire.Request = {
          val localRequestMap: HashMap[String, wire.Request] =
            t.head.map { pair =>
              (pKeyEncoder(pair._1), toWireA.value.toWire(pair._2))
            }
          autoT.value.toWire(t.tail) ++
            wire.Request.unit(kWitness.value, localRequestMap)
        }
      }

  }

}
