/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha

import scala.language.higherKinds

package object maia {

  type RequestMode = RequestMode.type
  type ResponseMode = ResponseMode.type

  type Request[A[_ <: Mode]] = A[RequestMode]
  type Response[A[_ <: Mode]] = A[ResponseMode]
  type Query[A[_ <: Mode]] = A[QueryMode[A]]
  type Fetcher[M[_], A[_ <: Mode]] = A[FetcherMode[M]]

}
