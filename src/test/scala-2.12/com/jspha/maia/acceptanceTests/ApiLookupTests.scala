/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests

import com.jspha.maia._
import com.jspha.maia.exampleApi._
import cats.data._
import cats.implicits._
import utest._
import utest.framework.{Test, Tree}

object ApiLookupTests extends TestSuite {

//  val lkUserNames: Lookup[Api, List[String]] =
//    Api.q.getAllUsers { user =>
//      user.name
//    }

  val lkUserNameAge: Lookup[Api, (String, Int)] =
    Api.q.getUser(User.JosephAbrahamson) { user =>
      (user.name |@| user.age).tupled
    }

  def runLookup[R](l: Lookup[Api, R]): Validated[LookupError, R] =
    l.handleResponse(Api.i(Api.fetcher, l.request))

  val tests: Tree[Test] = this {

    'lkUserNameAge {

      val result: Validated[LookupError, (String, Int)] =
        runLookup(lkUserNameAge)

      'success {
        assertMatch(result) { case Validated.Valid((_, _)) => }
      }

      val Validated.Valid((name, age)) = result

      'name { name ==> "Joseph Abrahamson" }
      'age { age ==> 29 }

    }

  }

}
