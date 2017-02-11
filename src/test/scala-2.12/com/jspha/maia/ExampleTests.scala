/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import utest._

object ExampleTests extends TestSuite {

  case class User[S <: Mode](
    name: S#Atom[String]
  )

  case class Api[S <: Mode](
    getUser: S#Obj[User]
  )

  val query: Query[Api] =
    implicitly[props.HasQuery[Api]].query

  val apiReq1: Request[Api] =
    Api[modes.RequestMode.type](None)

  val apiReq2: Request[Api] = {
    val user = User[modes.RequestMode.type](name = true)
    val api = Api[modes.RequestMode.type](getUser = Some(user))
    api
  }

  val apiResp1: Response[Api] =
    Api[modes.ResponseMode.type](None)

  val apiResp2: Response[Api] = {
    val user = User[modes.ResponseMode.type](name = Some("Joseph Abrahamson"))
    val api = Api[modes.ResponseMode.type](getUser = Some(user))
    api
  }

  val tests = this {

  }

}
