/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha

import scala.language.higherKinds

package object maia {

  type Request[A[_ <: Mode]] = A[Mode.Request]
  type Response[E, A[_ <: Mode]] = A[Mode.Response[E]]
  type Query[E, A[_ <: Mode]] = A[Mode.Query[A, E]]
  type Fetcher[M[_], E, A[_ <: Mode]] = A[Mode.Fetcher[M, E]]

}
