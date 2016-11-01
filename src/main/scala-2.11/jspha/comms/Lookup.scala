package jspha.comms

import cats.Applicative
import cats.instances.option._
import jspha.comms

/**
  * A Lookup is an Applicative describing both how to formulate a request to
  * the Comms server and how to interpret its response into a user-defined
  * datatype.
  */
//case class Lookup[T](req: Request, run: Response => Option[T])
//
//object Lookup {
//
//  implicit val LookupIsApplicative: Applicative[Lookup] =
//    new Applicative[Lookup] {
//      def pure[A](x: A): Lookup[A] =
//        Lookup(
//          req = comms.Request.empty,
//          run = _ => Some(x)
//        )
//
//      override def map[A, B](fa: Lookup[A])(f: A => B): Lookup[B] =
//        fa.copy(run = resp => fa.run(resp).map(f))
//
//      def ap[A, B](ff: Lookup[(A) => B])(fa: Lookup[A]): Lookup[B] =
//        Lookup(
//          req = ff.req ++ fa.req,
//          run = resp => Applicative[Option].ap(ff.run(resp))(fa.run(resp))
//        )
//    }
//
//}

