/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.util

import jspha.comms.{CSet, Cardinality}

trait Narrowing[C <: Cardinality] {
  def pf[A]: PartialFunction[CSet[A], Unit]
  def apply[A](set: CSet[A]): Option[C#Wrap[A]] =
    if (pf.isDefinedAt(set)) Some(set.asInstanceOf) else None
}

object Narrowing {

  import Cardinality._

  implicit val SingularNarrowing: Narrowing[Singular] =
    new Narrowing[Singular] {
      def pf[A] = { case CSet.Singular(_) => () }
    }

  implicit val OptionalNarrowing: Narrowing[Optional] =
    new Narrowing[Optional] {
      def pf[A] = { case CSet.Optional(_) => () }
    }

  implicit val VariableNarrowing: Narrowing[Variable] =
    new Narrowing[Variable] {
      def pf[A] = { case CSet.Variable(_) => () }
    }

}
