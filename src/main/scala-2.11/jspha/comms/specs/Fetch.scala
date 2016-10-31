package jspha.comms.specs

import cats.Traverse
import cats.data.Xor
import cats.instances.option._
import io.circe.Encoder
import jspha.comms._
import scala.language.higherKinds
import scala.reflect.runtime.universe.TypeTag

/**
  * Fetch is an implementation of Spec which helps create Lookup values.
  */
trait Fetch extends Qs {
  class Atomic[P: TypeTag, M <: Multiplicity, T: TypeTag](
      val name: String
  )(
      implicit mSelector: Multiplicity.MSelector[M],
      mTraverse: Traverse[M#Apply],
      pWriter: Encoder[P]
  ) {

    def get(implicit defaultParam: DefaultParam[P]): Lookup[M#Apply[T]] =
      getWithParam(defaultParam.value)

    def getWithParam(param: P): Lookup[M#Apply[T]] = {
      val k: Key.Dyn = Key(name, param).forget

      val req = Request.unit(k)
      def run(resp: Response): Option[M#Apply[T]] =
        resp(k) match {
          case None => None // missing key!
          case Some(Xor.Right(resps)) => None // expecting leaf!
          case Some(Xor.Left(dynMSet)) =>
            mSelector(dynMSet) match {
              case None => None // bad multiplicity!
              case Some(dyns) =>
                mTraverse.traverse[Option, Dyn, T](dyns)(_.open[T])
            }
        }

      Lookup(req, run)
    }

  }

  class Nested[P: TypeTag, M <: Multiplicity, T[_ <: Qs]](
      val name: String,
      next: => T[Fetch]
  )(
      implicit mSelector: Multiplicity.MSelector[M],
      mTraverse: Traverse[M#Apply],
      pWriter: Encoder[P]
  ) {

    def apply[R](cont: T[Fetch] => Lookup[R])(
        implicit defaultParam: DefaultParam[P]
    ): Lookup[M#Apply[R]] =
      getWithParam(defaultParam.value)(cont)

    def get[R](cont: T[Fetch] => Lookup[R])(
        implicit defaultParam: DefaultParam[P]
    ): Lookup[M#Apply[R]] =
      getWithParam(defaultParam.value)(cont)

    def getWithParam[R](param: P)(
        cont: T[Fetch] => Lookup[R]
    ): Lookup[M#Apply[R]] = {

      val lk: Lookup[R] = cont(next)
      val k: Key.Dyn = Key(name, param).forget

      val req = lk.req.nest(k)
      def run(resp: Response): Option[M#Apply[R]] =
        resp(k) match {
          case None => None // missing key!
          case Some(Xor.Left(dyns)) => None // expecting branch!
          case Some(Xor.Right(respMSet)) =>
            mSelector(respMSet) match {
              case None => None // bad multiplicity!
              case Some(resps) => mTraverse.traverse(resps)(lk.run)
            }
        }

      Lookup(req, run)
    }

  }

}

object Fetch extends Fetch {

  import shapeless._
  import shapeless.labelled._

  private[comms] trait Auto[T] {
    val value: T
  }

  private[comms] object Auto {
    implicit val FetchingHNil: Auto[HNil] = new Auto[HNil] {
      val value: HNil = HNil
    }

    implicit def FetchingAtomic[K <: Symbol,
                                P: TypeTag,
                                M <: Multiplicity,
                                A: TypeTag,
                                T <: HList](
        implicit kWitness: Witness.Aux[K],
        fetchingT: Lazy[Auto[T]],
        mSelector: Multiplicity.MSelector[M],
        mTraverse: Traverse[M#Apply],
        pWriter: Encoder[P]
    ): Auto[FieldType[K, Fetch#Atomic[P, M, A]] :: T] =
      new Auto[FieldType[K, Fetch#Atomic[P, M, A]] :: T] {
        val value: FieldType[K, Fetch#Atomic[P, M, A]] :: T =
          field[kWitness.T](new Fetch.Atomic[P, M, A](kWitness.value.name)) ::
            fetchingT.value.value
      }

    implicit def FetchingNested[K <: Symbol,
                                P: TypeTag,
                                M <: Multiplicity,
                                A[_ <: Qs],
                                T <: HList](
        implicit kWitness: Witness.Aux[K],
        fetchingT: Lazy[Auto[T]],
        fetchingA: Lazy[Auto[A[Fetch]]],
        mSelector: Multiplicity.MSelector[M],
        mTraverse: Traverse[M#Apply],
        pWriter: Encoder[P]
    ): Auto[FieldType[K, Fetch#Nested[P, M, A]] :: T] =
      new Auto[FieldType[K, Fetch#Nested[P, M, A]] :: T] {
        val value: FieldType[K, Fetch#Nested[P, M, A]] :: T =
          field[kWitness.T](
            new Fetch.Nested[P, M, A](kWitness.value.name,
                                      fetchingA.value.value)
          ) :: fetchingT.value.value
      }

    implicit def FetchingGeneric[T[_ <: Qs], Repr <: HList](
        implicit gen: LabelledGeneric.Aux[T[Fetch], Repr],
        fetchRepr: Lazy[Auto[Repr]]
    ): Auto[T[Fetch]] =
      new Auto[T[Fetch]] {
        val value: T[Fetch] = gen.from(fetchRepr.value.value)
      }

  }

}
