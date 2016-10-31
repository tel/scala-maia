package jspha.comms.interpretation

import cats.Traverse
import cats.data.Xor
import cats.instances.option._
import io.circe.Encoder

import scala.reflect.runtime.universe.TypeTag
import jspha.comms.Api
import jspha.comms.api.Multiplicity
import jspha.comms.wire.{Dyn, Key, Request, Response}

sealed trait Fetcher

case class Here[P: Encoder, M <: Multiplicity, T: TypeTag](name: String)(
    implicit mSelector: Multiplicity.MSelector[M],
    mTraverse: Traverse[M#Apply]
) extends Fetcher {

  def get(param: P): Lookup[M#Apply[T]] = {
    val k: Key = Key(name, param)

    val req = Request.unit(k)
    def run(resp: Response): Option[M#Apply[T]] =
      resp(k) match {
        case None => None // missing key!
        case Some(Xor.Right(resps)) => None // expecting leaf!
        case Some(Xor.Left(dynMSet)) =>
          mSelector(dynMSet) match {
            case None => None // bad multiplicity!
            case Some(dyns) => mTraverse.traverse[Option, Dyn, T](dyns)(_.open)
          }
      }

    Lookup(req, run)
  }

}

case class There[P: Encoder, M <: Multiplicity, A <: Api](
    q: Querying[A],
    name: String
)(
    implicit mSelector: Multiplicity.MSelector[M],
    mTraverse: Traverse[M#Apply]
) extends Fetcher {

  def get[T](param: P)(
    cont: Querying.Natural[q.Out] => Lookup[T]
  ): Lookup[M#Apply[T]] = {

    val lk: Lookup[T] = cont(Querying.Natural(q.value))
    val k: Key = Key(name, param)

    val req = lk.req.nest(k)
    def run(resp: Response): Option[M#Apply[T]] =
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
