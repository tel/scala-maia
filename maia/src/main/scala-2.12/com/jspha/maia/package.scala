/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha

import scala.language.higherKinds

package object maia {

  type Request[T[_ <: Dsl]] = T[form.Request]
  type Response[T[_ <: Dsl]] = T[form.Response]
  type QueriesAt[T[_ <: Dsl]] = T[form.QueriesAt[T]]
  type Handler[F[_], T[_ <: Dsl]] = T[form.Handler[F]]

}
