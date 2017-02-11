/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.modes

import scala.language.higherKinds
import com.jspha.maia._

class QueryMode[Super[_ <: Mode]] extends Mode {
  case class Atom[A](
    request: Request[Super],
    response: Response[Super] => A
  ) {
    def get: Lookup[Super, A] =
      Lookup[Super, A](request, response)
  }

  case class Obj[Sub[_ <: Mode]](
    request: Request[Sub] => Request[Super],
    response: Response[Super] => Response[Sub],
    sub: Query[Sub]
  ) {
    def get[R](cont: Query[Sub] => Lookup[Sub, R]): Lookup[Super, R] = {
      val subLok = cont(sub)
      Lookup[Super, R](
        request(subLok.print),
        response andThen subLok.parse
      )
    }
  }
}
