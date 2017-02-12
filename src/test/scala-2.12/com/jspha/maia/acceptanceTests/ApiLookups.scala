/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests

import cats.implicits._
import com.jspha.maia._
import com.jspha.maia.acceptanceTests.exampleApi.{Api, User}

object ApiLookups {

  val o1: Option[Int] = Some(3)
  val o2: Option[Int] = Some(3)
  val oo: Option[(Int, Int)] = (o1 |@| o2).tupled

  val lkUserName: Lookup[Api, String] =
    Api.q.getUser.get { user =>
      user.name.get
    }

  val lkUserNameAge: Lookup[Api, (String, Int)] =
    Api.q.getUser.get { user =>
      val name: Lookup[User, String] = user.name.get
      val age: Lookup[User, Int] = user.age.get
      val tup: Lookup[User, (String, Int)] = (name |@| age).tupled
      tup
    }

}
