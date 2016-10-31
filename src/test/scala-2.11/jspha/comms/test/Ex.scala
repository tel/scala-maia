package jspha.comms.test

import cats.Traverse
import cats.data.Xor
import cats.instances.option._
import io.circe.Encoder
import jspha.comms.interpretation.Lookup
import jspha.comms.wire.{Dyn, Key, Request, Response}
import shapeless._
import shapeless.labelled._

import scala.language.higherKinds
import utest._

object Ex extends TestSuite {

  import jspha.comms.api.{Multiplicity => Mul}

  trait DefaultParam[T] {
    val value: T
  }

  object DefaultParam {

    def apply[T](t: T): DefaultParam[T] = new DefaultParam[T] {
      val value: T = t
    }

    implicit val DefaultParamUnit: DefaultParam[Unit] = apply(())

  }

  trait Pres {
    type Atomic[P, M <: Mul, T]
    type Nested[P, M <: Mul, T[_ <: Pres]]

    type Atomic1[T] = Atomic[Unit, Mul.One, T]
    type Nested1[T[_ <: Pres]] = Nested[Unit, Mul.One, T]

    type A[P, M <: Mul, T] = Atomic[P, M, T]
    type N[P, M <: Mul, T[_ <: Pres]] = Nested[P, M, T]

    type A1[T] = Atomic1[T]
    type N1[T[_ <: Pres]] = Nested1[T]
  }

//  object Concrete extends Concrete
//  trait Concrete extends Pres {
//    type Atomic[P, M <: Mul, T] = P => M#Apply[T]
//    type Nested[P, M <: Mul, T[_ <: Pres]] = ()
//  }

  object Fetch extends Fetch
  trait Fetch extends Pres {
    class Atomic[P, M <: Mul, T](
        val name: String
    )(
        implicit mSelector: Mul.MSelector[M],
        mTraverse: Traverse[M#Apply],
        pWriter: Encoder[P]
    ) {

      def get(implicit defaultParam: DefaultParam[P]): Lookup[M#Apply[T]] =
        getWithParam(defaultParam.value)

      def getWithParam(param: P): Lookup[M#Apply[T]] = {
        val k: Key = Key(name, param)

        val req = Request.unit(k)
        def run(resp: Response): Option[M#Apply[T]] =
          resp(k) match {
            case None => None // missing key!
            case Some(Xor.Right(resps)) => None // expecting leaf!
            case Some(Xor.Left(dynMSet)) =>
              mSelector(dynMSet) match {
                case None => None // bad multiplicity!
                case Some(dyns) =>
                  mTraverse.traverse[Option, Dyn, T](dyns)(_.open)
              }
          }

        Lookup(req, run)
      }

    }

    class Nested[P, M <: Mul, T[_ <: Pres]](
        val name: String,
        next: => T[Fetch]
    )(
        implicit mSelector: Mul.MSelector[M],
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
        val k: Key = Key(name, param)

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

  trait Fetching[T] {
    def value: T
  }

  object Fetching {
    implicit val FetchingHNil: Fetching[HNil] = new Fetching[HNil] {
      def value: HNil = HNil
    }

    implicit def FetchingAtomic[K <: Symbol, P, M <: Mul, A, T <: HList](
        implicit kWitness: Witness.Aux[K],
        fetchingT: Lazy[Fetching[T]],
        mSelector: Mul.MSelector[M],
        mTraverse: Traverse[M#Apply],
        pWriter: Encoder[P]
    ): Fetching[FieldType[K, Fetch#Atomic[P, M, A]] :: T] =
      new Fetching[FieldType[K, Fetch#Atomic[P, M, A]] :: T] {
        def value: FieldType[K, Fetch#Atomic[P, M, A]] :: T =
          field[kWitness.T](new Fetch.Atomic[P, M, A](kWitness.value.name)) ::
            fetchingT.value.value
      }

    implicit def FetchingNested[K <: Symbol,
                                P,
                                M <: Mul,
                                A[_ <: Pres],
                                T <: HList](
        implicit kWitness: Witness.Aux[K],
        fetchingT: Lazy[Fetching[T]],
        fetchingA: Lazy[Fetching[A[Fetch]]],
        mSelector: Mul.MSelector[M],
        mTraverse: Traverse[M#Apply],
        pWriter: Encoder[P]
    ): Fetching[FieldType[K, Fetch#Nested[P, M, A]] :: T] =
      new Fetching[FieldType[K, Fetch#Nested[P, M, A]] :: T] {
        def value: FieldType[K, Fetch#Nested[P, M, A]] :: T =
          field[kWitness.T](
            new Fetch.Nested[P, M, A](kWitness.value.name,
                                      fetchingA.value.value)
          ) :: fetchingT.value.value
      }

    implicit def FetchingGeneric[T[_ <: Pres], Repr <: HList](
        implicit gen: LabelledGeneric.Aux[T[Fetch], Repr],
        fetchRepr: Lazy[Fetching[Repr]]
    ): Fetching[T[Fetch]] =
      new Fetching[T[Fetch]] {
        def value: T[Fetch] = gen.from(fetchRepr.value.value)
      }

  }

  case class Loc[S <: Pres](
      lat: S#A1[Double],
      lon: S#A1[Double]
  )
  case class City[S <: Pres](
      name: S#A1[String],
      loc: S#N1[Loc],
      mayor: S#N1[User]
  )
  case class User[S <: Pres](
      home: S#N1[City],
      loc: S#N1[Loc],
      name: S#A1[String]
  )
  case class Query[S <: Pres](
      currentUser: S#N1[User]
  )

  val qFetch: Query[Fetch] =
    the[Fetching[Query[Fetch]]].value

  val lookup = qFetch.currentUser { u =>
    import cats.syntax.cartesian._

    val lLoc = u.loc { l =>
      (l.lat.get |@| l.lon.get).tupled
    }

    (u.name.get |@| lLoc).tupled
  }

  val tests = this {

//    'qFetch {
//      * - { qFetch.currentUser ==> "currentUser" }
//    }

  }

}
