/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.examples.api1

import com.jspha.maia._
import cats._
import com.jspha.maia.examples.util.{CirceSerialization => Csz}
import io.circe._

final case class User[M <: Dsl](
  name: M#Atom[String],
  age: M#Atom[Int],
  hometown: M#Obj[City],
  mother: M#ObjK[User, NoArg, NoErr, Opt],
  lastKnownLocation: M#Obj[Location],
  getId: M#Obj[Identity]
)

object User {

  sealed trait UID
  case object Root extends UID
  case object JosephAbrahamson extends UID

  object UID {
    implicit val encoder: Encoder[UID] =
      io.circe.generic.semiauto.deriveEncoder
    implicit val decoder: Decoder[UID] =
      io.circe.generic.semiauto.deriveDecoder
  }

  def fetch(id: UID): Handler[Id, User] = id match {
    case Root =>
      User[form.Handler[Id]](
        name = "Root",
        age = -1,
        hometown = City.atlanta,
        mother = None,
        lastKnownLocation = Location.fetchConst(0, 0),
        getId = Identity.fetcher("root")
      )
    case JosephAbrahamson =>
      User[form.Handler[Id]](
        name = "Joseph Abrahamson",
        age = 29,
        hometown = City.atlanta,
        mother = None,
        lastKnownLocation = Location.fetchConst(42.3601, 71.0589),
        getId = Identity.fetcher("joseph-abrahamson")
      )
  }

  val req0: Request[User] =
    typelevel.NullRequest[User]

  val req1: Request[User] =
    User[form.Request](
      name = true,
      age = true,
      hometown = None,
      mother = Some(
        User[form.Request](
          name = true,
          age = false,
          hometown = None,
          mother = None,
          lastKnownLocation = None,
          getId = None
        )),
      lastKnownLocation = Some(Location.req0),
      getId = Some(Identity.req0)
    )

  val reqCombined: Request[User] =
    typelevel.MergeRequests[User](req0, req1)

  implicit lazy val q: QueriesAt[User] =
    typelevel.GetQueriesAt[User]

  def runner(req: Request[User]): Response[User] =
    typelevel.RunHandler[Id, User](fetch(Root), req)

  lazy val sz: Serializer[Csz.Params, User] =
    User[form.Serializer[Csz.Params]](
      name = form.Serializer
        .Atom[Csz.Params, String, NoArg, NoErr, One]((), (), Csz.circeSection),
      age = form.Serializer
        .Atom[Csz.Params, Int, NoArg, NoErr, One]((), (), Csz.circeSection),
      hometown =
        form.Serializer.Obj[Csz.Params, City, NoArg, NoErr, One]((),
                                                                 (),
                                                                 City.sz),
      mother =
        form.Serializer.Obj[Csz.Params, User, NoArg, NoErr, Opt]((), (), sz),
      lastKnownLocation = form.Serializer
        .Obj[Csz.Params, Location, NoArg, NoErr, One]((), (), Location.sz),
      getId = form.Serializer
        .Obj[Csz.Params, Identity, NoArg, NoErr, One]((), (), Identity.sz)
    )

}
