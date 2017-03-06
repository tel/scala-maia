/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests

import cats.implicits._
import com.jspha.maia._
import com.jspha.maia.exampleApi.{Api, User}

object ApiLookups {

  val lkUserName: Lookup[Api, String] =
    Api.q.getUser(User.Default) { user =>
      user.name
    }

  val lkUserNameAge: Lookup[Api, (String, Int)] =
    Api.q.getUser(User.JosephAbrahamson) { user =>
      (user.name |@| user.age).tupled
    }

}
