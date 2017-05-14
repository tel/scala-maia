/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import com.jspha.maia._
import cats._
import com.jspha.maia.examples.util.{CirceSerialization => Csz}

final case class City[F <: Dsl](
  name: F#Atom[String],
  location: F#Obj[Location],
  mayor: F#ObjK[User, NoArg, NoErr, Opt]
)

object City {

  def atlanta: Handler[Id, City] =
    City[form.Handler[Id]](
      name = "Atlanta",
      location = Location.fetchConst(33.7490, 84.3880),
      mayor = None
    )

  val req0: Request[City] =
    typelevel.NullRequest[City]

  val req1: Request[City] =
    City[form.Request](
      name = true,
      location = None,
      mayor = None
    )

  val reqCombined: Request[City] =
    typelevel.MergeRequests[City](req0, req1)

  val q: QueriesAt[City] =
    typelevel.GetQueriesAt[City]

  def runner(req: Request[City]): Response[City] =
    typelevel.RunHandler[Id, City](atlanta, req)

  lazy val sz: Serializer[Csz.Params, City] =
    City[form.Serializer[Csz.Params]](
      name = form.Serializer
        .Atom[Csz.Params, String, NoArg, NoErr, One]((), (), Csz.circeSection),
      location = form.Serializer
        .Obj[Csz.Params, Location, NoArg, NoErr, One]((), (), Location.sz),
      mayor = form.Serializer.Obj[Csz.Params, User, NoArg, NoErr, Opt]((),
                                                                       (),
                                                                       User.sz)
    )

}
