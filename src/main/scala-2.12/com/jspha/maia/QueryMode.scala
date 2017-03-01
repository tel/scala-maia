/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats.data.Validated

import scala.language.higherKinds

class QueryMode[Super[_ <: Mode]] extends Mode {
  case class Atom[A](
    name: Symbol,
    buildRequest: Request[Super],
    analyzeResponse: Response[Super] => Validated[LookupError, A]
  ) {
    def get: Lookup[Super, A] =
      Lookup[Super, A](buildRequest, analyzeResponse)
  }

  case class Obj[Sub[_ <: Mode]](
    name: Symbol,
    buildRequest: Request[Sub] => Request[Super],
    analyzeResponse: Response[Super] => Validated[LookupError, Response[Sub]],
    subQuery: Query[Sub]
  ) {
    def get[R](cont: Query[Sub] => Lookup[Sub, R]): Lookup[Super, R] = {
      val subLok: Lookup[Sub, R] = cont(subQuery)
      Lookup[Super, R](
        buildRequest(subLok.buildRequest),
        resp => {
          // We want a `flatMap`-like operation here, but this is not
          // provided (for good reason) on `Validated`... so we do it manually
          analyzeResponse(resp) match {
            case Validated.Invalid(err) => Validated.Invalid(err)
            case Validated.Valid(subResp) =>
              // We mark the lower errors with an "object group name" forming
              // a trie of errors
              subLok
                .analyzeResponse(subResp)
                .leftMap(LookupError.Nested(name, _))
          }
        }
      )
    }
  }
}
