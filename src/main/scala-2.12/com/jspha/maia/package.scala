/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha

import scala.language.higherKinds

package object maia {

  type Request[A[_ <: Fields]] = A[Fields.Request]
  type Response[A[_ <: Fields]] = A[Fields.Response]
  type Query[A[_ <: Fields]] = A[Fields.Query[A]]
  type Fetcher[M[_], A[_ <: Fields]] = A[Fields.Fetcher[M]]

  type LookupS[Api[_ <: Fields], A] = Lookup[Api, Nothing, A]

}
