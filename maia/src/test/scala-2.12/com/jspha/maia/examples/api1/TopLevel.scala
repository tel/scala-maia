/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import cats._
import com.jspha.maia._
import com.jspha.maia.examples.util.{CirceSerialization => Csz}

import scala.collection.immutable.HashMap

final case class TopLevel[F <: Dsl](
  getRoot: F#Obj[User],
  getUser: F#ObjK[User, HasArg[User.UID], NoErr, One],
  getAllUsers: F#ObjK[User, NoArg, NoErr, Many]
)

object TopLevel {

  val fetcher: Handler[Id, TopLevel] =
    TopLevel[form.Handler[Id]](
      getRoot = User.fetch(User.Root),
      getUser = (id: User.UID) => User.fetch(id),
      getAllUsers = List(
        User.fetch(User.Root),
        User.fetch(User.JosephAbrahamson)
      )
    )

  val req0: Request[TopLevel] =
    typelevel.NullRequest[TopLevel]

  val req1: Request[TopLevel] =
    TopLevel[form.Request](
      getRoot = None,
      getUser = HashMap(User.JosephAbrahamson -> User.req1),
      getAllUsers = None
    )

  val reqCombined: Request[TopLevel] =
    typelevel.MergeRequests[TopLevel](req0, req1)

  val q: QueriesAt[TopLevel] =
    typelevel.GetQueriesAt[TopLevel]

  def runner(req: Request[TopLevel]): Response[TopLevel] =
    typelevel.RunHandler[Id, TopLevel](fetcher, req)

  lazy val sz: Serializer[Csz.Params, TopLevel] =
    TopLevel[form.Serializer[Csz.Params]](
      getRoot = form.Serializer.Obj[Csz.Params, User, NoArg, NoErr, One]((), (), User.sz),
      getUser = form.Serializer.Obj[Csz.Params, User, HasArg[User.UID], NoErr, One](Csz.circeSection, (), User.sz),
      getAllUsers = form.Serializer.Obj[Csz.Params, User, NoArg, NoErr, Many]((), (), User.sz)
    )

}
