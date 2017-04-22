/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.http4s

import com.jspha.maia._
import fs2.Task
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.{Request => _, Response => _, _}

import scala.language.higherKinds

class ApiEndpoint[M[_], T[_ <: Dsl]](
  fetcher: Handler[M, T],
  asTask: M[Response[T]] => Task[Response[T]])(
  implicit
//  reqDecoder: RequestDecoder[T],
//  respEncoder: ResponseEncoder[T],
  runner: typelevel.RunHandler[M, T]) {

  implicit val reqEntityDecoder: EntityDecoder[Request[T]] =
    ??? // jsonOf(reqDecoder)

  implicit val respEntityEncoder: EntityEncoder[Response[T]] =
    ??? // jsonEncoderOf(respEncoder)

  val service = HttpService {
    case req @ POST -> Root =>
      for {
        maiaReq <- req.as[Request[T]]
        maiaResp <- asTask(runner(fetcher, maiaReq))
        http4sResp <- Ok(maiaResp)
      } yield http4sResp
  }

}

object ApiEndpoint {

  def apply[M[_], T[_ <: Dsl]](fetcher: Handler[M, T],
                               asTask: M[Response[T]] => Task[Response[T]])(
    implicit
//    reqDecoder: RequestDecoder[T],
//    respEncoder: ResponseEncoder[T],
    interprets: typelevel.RunHandler[M, T]): ApiEndpoint[M, T] =
    new ApiEndpoint[M, T](fetcher, asTask)

  def apply[T[_ <: Dsl]](fetcher: Handler[Task, T])(
    implicit
//    reqDecoder: RequestDecoder[T],
//    respEncoder: ResponseEncoder[T],
    interprets: typelevel.RunHandler[Task, T]): ApiEndpoint[Task, T] =
    new ApiEndpoint[Task, T](fetcher, identity)

}
