/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import cats.Id
import com.jspha.maia._

final case class Identity[F <: Dsl](
  keyName: F#Atom[String],
  secret: F#AtomK[String, NoArg, HasErr[Identity.Error], One]
)

object Identity {

  sealed trait Error
  case object NotAuthorized extends Error

  def fetcher(name: String): Handler[Id, Identity] =
    Identity[form.Handler[Id]](
      keyName = name,
      secret = Left(NotAuthorized)
    )

  val req0: Request[Identity] =
    typelevel.NullRequest[Identity]

  implicit val q: QueriesAt[Identity] =
    typelevel.GetQueriesAt[Identity]

  def runner(req: Request[Identity]): Response[Identity] =
    typelevel.RunHandler[Id, Identity](fetcher("foo"), req)

}
