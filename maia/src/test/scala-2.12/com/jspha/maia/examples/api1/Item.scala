/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import cats.Id
import com.jspha.maia._

final case class Item[F <: Dsl](
  name: F#Atom[String],
  cost: F#AtomK[Double, HasArg[Item.Currency], HasErr[Item.Error], One]
)

object Item {

  sealed trait Currency
  case object USD extends Currency

  sealed trait Error
  case object NotAvailable extends Error

  def fetchConst(name: String, cost: Currency => Double): Handler[Id, Item] =
    Item[form.Handler[Id]](
      name = name,
      cost = curr => Right(cost(curr))
    )

  val req0: Request[Item] =
    typelevel.NullRequest[Item]

  val q: QueriesAt[Item] =
    typelevel.GetQueriesAt[Item]

  def runner(req: Request[Item]): Response[Item] =
    typelevel.RunHandler[Id, Item](fetchConst("foo", _ => 10), req)
}
