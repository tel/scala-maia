/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.util

import jspha.comms._

trait Narrowing[C <: Cardinality] {
  def apply[A](set: CSet[A]): Option[C#Wrap[A]]
}

object Narrowing {

  import Cardinality._

  implicit val ofSingular: Narrowing[Singular] =
    new Narrowing[Singular] {
      def apply[A](set: CSet[A]): Option[CSet.Singular[A]] = set match {
        case CSet.Singular(v) => Some(set.asInstanceOf[CSet.Singular[A]])
        case _ => None
      }
    }

  implicit val ofOptional: Narrowing[Optional] =
    new Narrowing[Optional] {
      def apply[A](set: CSet[A]): Option[CSet.Optional[A]] = set match {
        case CSet.Optional(v) => Some(set.asInstanceOf[CSet.Optional[A]])
        case _ => None
      }
    }

  implicit val ofVariable: Narrowing[Variable] =
    new Narrowing[Variable] {
      def apply[A](set: CSet[A]): Option[CSet.Variable[A]] = set match {
        case CSet.Variable(v) => Some(set.asInstanceOf[CSet.Variable[A]])
        case _ => None
      }
    }

}
