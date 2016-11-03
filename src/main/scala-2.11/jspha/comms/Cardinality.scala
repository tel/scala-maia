/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import scala.language.higherKinds

sealed trait Cardinality {
  type Wrap[A] <: CSet[A]
}

object Cardinality {
  sealed trait Singular extends Cardinality { type Wrap[A] = CSet.Singular[A] }
  sealed trait Optional extends Cardinality { type Wrap[A] = CSet.Optional[A] }
  sealed trait Variable extends Cardinality { type Wrap[A] = CSet.Variable[A] }
}
