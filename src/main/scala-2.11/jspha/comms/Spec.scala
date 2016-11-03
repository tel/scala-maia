/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import scala.language.higherKinds

/**
  * A trait extending Spec describes how to fetch either an atomic or nested
  * type from the described Api.
  */
trait Spec {
  type Atomic[P, M <: Cardinality, T]
  type Nested[P, M <: Cardinality, T[_ <: Spec]]
}
