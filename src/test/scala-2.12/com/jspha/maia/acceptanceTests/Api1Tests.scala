/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.acceptanceTests

import com.jspha.maia._
import com.jspha.maia.examples.api1._
import cats.data._
import cats.implicits._
import cats.syntax._
import utest._
import utest.framework.{Test, Tree}

object Api1Tests extends TestSuite {

  val lkUserNames: Lookup[TopLevel, Nothing, List[String]] =
    TopLevel.q.getAllUsers { user =>
      user.name
    }

//  val lkUserNameAge: Lookup[TopLevel, Nothing, (String, Int)] =
//    TopLevel.q.getUser(User.JosephAbrahamson) { user =>
//      (catsSyntaxCartesian(user.name) |@| user.age).tupled
//    }

  def runLookup[R, E](
    l: Lookup[TopLevel, E, R]): Validated[LookupError[E], R] =
    l.handleResponse(TopLevel.i(TopLevel.fetcher, l.request))

  val tests: Tree[Test] = this {

//    'lkUserNameAge {
//
//      val result: Validated[LookupError[Nothing], (String, Int)] =
//        runLookup(lkUserNameAge)
//
//      'success {
//        assertMatch(result) { case Validated.Valid((_, _)) => }
//      }
//
//      val Validated.Valid((name, age)) = result
//
//      'name { name ==> "Joseph Abrahamson" }
//      'age { age ==> 29 }
//
//    }

  }

}
