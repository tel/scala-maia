package jspha.comms.interpretation

import cats.Traverse
import io.circe.Encoder
import jspha.comms.Api

import scala.language.dynamics
import scala.reflect.runtime.universe.TypeTag
import jspha.comms.api.{Multiplicity => Mul}
import jspha.comms.api.{Presentation => Pres}
import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

trait Querying[T] {

  type Out <: HList
  val value: Out
  val natural: Querying.Natural[Out] =
    Querying.Natural(value)

}

object Querying {

  /**
    * Natural queries enable "dot" style access.
    */
  final case class Natural[L <: HList](l: L) extends Dynamic {
    import shapeless.ops.record._
    import shapeless.tag._

    def selectDynamic(key: String)
      (implicit sel: Selector[L, Symbol @@ key.type]): sel.Out = sel(l)
  }

  type Aux[T, S] = Querying[T] { type Out = S }

  implicit val HNilSelector: Aux[HNil, HNil] = new Querying[HNil] {
    type Out = HNil
    val value: Out = HNil
  }

  implicit def HConsSelectorAtomic[K <: Symbol, P, M <: Mul, A, T <: HList](
      implicit kWitness: Witness.Aux[K],
      tSelector: Lazy[Querying[T]],
      typeTagA: TypeTag[A],
      mSelector: Mul.MSelector[M],
      mTraverse: Traverse[M#Apply],
      pWriter: Encoder[P]
  ): Aux[
    FieldType[K, Pres.Atomic[P, M, A]] :: T,
    FieldType[K, Here[P, M, A]] :: tSelector.value.Out
  ] =
    new Querying[FieldType[K, Pres.Atomic[P, M, A]] :: T] {
      type Out =
        FieldType[K, Here[P, M, A]] :: tSelector.value.Out
      val value: Out =
        field[kWitness.T](Here[P, M, A](kWitness.value.name)) ::
          tSelector.value.value
    }

  implicit def HConsSelectorNested[K <: Symbol,
                                   P,
                                   M <: Mul,
                                   A <: Api,
                                   T <: HList](
      implicit kWitness: Witness.Aux[K],
      tSelector: Lazy[Querying[T]],
      aSelector: Lazy[Querying[A]],
      mSelector: Mul.MSelector[M],
      mTraverse: Traverse[M#Apply],
      pWriter: Encoder[P]
  ): Aux[
    FieldType[K, Pres.Nested[P, M, A]] :: T,
    FieldType[K, There[P, M, A]] :: tSelector.value.Out
  ] =
    new Querying[FieldType[K, Pres.Nested[P, M, A]] :: T] {
      type Out =
        FieldType[K, There[P, M, A]] :: tSelector.value.Out
      val value: Out =
        field[kWitness.T](There[P, M, A](aSelector.value, kWitness.value.name)) ::
          tSelector.value.value
    }

  implicit def LabelledGenericQuerying[T, Repr <: HList](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    selRepr: Lazy[Querying[Repr]]
  ): Aux[T, selRepr.value.Out] =
    new Querying[T] {
      type Out = selRepr.value.Out
      val value: Out = selRepr.value.value
    }

}
