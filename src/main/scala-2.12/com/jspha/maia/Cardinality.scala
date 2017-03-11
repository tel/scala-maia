/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._
import cats.implicits._

import scala.language.higherKinds

sealed trait Cardinality {
  type Coll[_]
}

object Cardinality {

  sealed trait One extends Cardinality { type Coll[A] = A }
  sealed trait Opt extends Cardinality { type Coll[A] = Option[A] }
  sealed trait Many extends Cardinality { type Coll[A] = List[A] }

  trait Ops[M <: Cardinality] {
    val traversable: Traverse[M#Coll]
  }

  object Ops {
    def apply[M <: Cardinality](implicit T: Traverse[M#Coll]): Ops[M] =
      new Ops[M] {
        val traversable: Traverse[M#Coll] = T
      }
  }

  implicit val SingularOps: Ops[One] = Ops[One]
  implicit val OptionalOps: Ops[Opt] = Ops[Opt]
  implicit val CollectionOps: Ops[Many] = Ops[Many]

}
