/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.util

import cats._
import com.jspha.maia.serialization._
import io.circe._
import io.circe.syntax._

object CirceSerialization {

  sealed trait Params extends SerializationParams {
    type Target = Json
    type InjectEff[A] = A
    type RetractEff[A] = Either[DecodingFailure, A]
  }

  /**
    * Wrapper used for encoding [[com.jspha.maia.One#Coll]] values.
    * Without this extra layer it becomes impossible to uniquely decode
    * certain values.
    */
  final case class The[A](value: A) extends AnyVal

  object The {
    implicit def theEncoder[A](implicit enc: Encoder[A]): Encoder[The[A]] =
      (a: The[A]) => Map("the" -> a.value).asJson

    implicit def theDecoder[A](implicit dec: Decoder[A]): Decoder[The[A]] =
      (c: HCursor) => c.downField("the").as[A].map(The(_))
  }

  implicit object circeOps extends SerializationOps[Params] {
    val obj: Section[Params, Map[String, Json]] =
      Section[Params, Map[String, Json]](s => s.asJson, j => j.as[Map[String, Json]])
    val params: Section[Params, Vector[(Json, Json)]] =
      Section[Params, Vector[(Json, Json)]](s => s.asJson, j => j.as[Vector[(Json, Json)]])
    val ofOne: Section[Params, Json] =
      Section[Params, Json](s => The(s).asJson,
                            j => j.as[The[Json]] map (_.value))
    val ofOpt: Section[Params, Option[Json]] =
      Section[Params, Option[Json]](s => s.asJson, j => j.as[Option[Json]])
    val ofMany: Section[Params, List[Json]] =
      Section[Params, List[Json]](s => s.asJson, j => j.as[List[Json]])
    val unit: Section[Params, Unit] =
      Section[Params, Unit](
        _ => true.asJson,
        j =>
          j.as[Boolean]
            .flatMap(
              b =>
                if (b)
                  Right(())
                else
                  Left(DecodingFailure("Expected true", List()))))
    implicit val injectMonad: Monad[Id] = Monad[Id]
    implicit val retractMonad: Monad[Either[DecodingFailure, ?]] =
      Monad[Either[DecodingFailure, ?]]
  }

  def circeSection[A](implicit encoder: Encoder[A],
                      decoder: Decoder[A]): Section[Params, A] =
    Section[Params, A](
      inject = encoder.apply,
      retract = decoder.decodeJson
    )

}
