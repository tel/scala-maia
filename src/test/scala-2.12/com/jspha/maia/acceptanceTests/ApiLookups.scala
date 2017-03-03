/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests

import cats.implicits._
import com.jspha.maia._
import com.jspha.maia.exampleApi.{Api, User}

object ApiLookups {

  val lkUserName: Lookup[Api, String] =
    Api.q.getUser { user =>
      user.name
    }

  val lkUserNameAge: Lookup[Api, (String, Int)] =
    // NOTE: This both (a) checks and (b) even infers just fine, but IntelliJ
    // can't handle it and throws red.
    // TODO: Let IntelliJ burn and remove the annotations to make the test
    // stronger
    Api.q.getUser { user =>
      val name: Lookup[User, String] = user.name
      val age: Lookup[User, Int] = user.age
      val tup: Lookup[User, (String, Int)] = (name |@| age).tupled
      tup
    }

}
