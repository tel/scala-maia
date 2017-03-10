/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._
import cats.implicits._

import scala.language.higherKinds

sealed trait Multiplicity {
  type Coll[_]
}

object Multiplicity {

  sealed trait Singular extends Multiplicity { type Coll[A] = A }
  sealed trait Optional extends Multiplicity { type Coll[A] = Option[A] }
  sealed trait Collection extends Multiplicity { type Coll[A] = List[A] }

  trait Ops[M <: Multiplicity] {
    val traversable: Traverse[M#Coll]
  }

  object Ops {
    def apply[M <: Multiplicity](implicit T: Traverse[M#Coll]): Ops[M] =
      new Ops[M] {
        val traversable: Traverse[M#Coll] = T
      }
  }

  implicit val SingularOps: Ops[Singular] = Ops[Singular]
  implicit val OptionalOps: Ops[Optional] = Ops[Optional]
  implicit val CollectionOps: Ops[Collection] = Ops[Collection]

}
