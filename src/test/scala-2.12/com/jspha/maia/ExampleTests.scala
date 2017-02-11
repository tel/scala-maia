/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import utest._
import utest.framework.{Test, Tree}

object ExampleTests extends TestSuite {

  case class User[S <: Mode](
    name: S#Atom[String]
  )

  case class Api[S <: Mode](
    getUser: S#Obj[User]
  )

  val tests: Tree[Test] = this {}

}
