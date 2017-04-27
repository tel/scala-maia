/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import com.jspha.maia._
import com.jspha.maia.examples.util.{CirceSerialization => Csz}
import cats.Id
import io.circe._

final case class Item[F <: Dsl](
  name: F#Atom[String],
  cost: F#AtomK[Double, HasArg[Item.Currency], HasErr[Item.Error], One]
)

object Item {

  sealed trait Currency
  case object USD extends Currency

  object Currency {
    implicit val encoder: Encoder[Currency] =
      io.circe.generic.semiauto.deriveEncoder
    implicit val decoder: Decoder[Currency] =
      io.circe.generic.semiauto.deriveDecoder
  }

  sealed trait Error
  case object NotAvailable extends Error

  object Error {
    implicit val encoder: Encoder[Error] =
      io.circe.generic.semiauto.deriveEncoder
    implicit val decoder: Decoder[Error] =
      io.circe.generic.semiauto.deriveDecoder
  }

  def fetchConst(name: String, cost: Currency => Double): Handler[Id, Item] =
    Item[form.Handler[Id]](
      name = name,
      cost = curr => Right(cost(curr))
    )

  val req0: Request[Item] =
    typelevel.NullRequest[Item]

  val req1: Request[Item] =
    Item[form.Request](
      name = false,
      cost = Set(USD)
    )

  val reqCombined: Request[Item] =
    typelevel.MergeRequests[Item](req0, req1)

  val q: QueriesAt[Item] =
    typelevel.GetQueriesAt[Item]

  def runner(req: Request[Item]): Response[Item] =
    typelevel.RunHandler[Id, Item](fetchConst("foo", _ => 10), req)

  val sz: Serializer[Csz.Params, Item] =
    Item[form.Serializer[Csz.Params]](
      name = ((), (), Csz.circeSection),
      cost = (Csz.circeSection, Csz.circeSection, Csz.circeSection)
    )

}
