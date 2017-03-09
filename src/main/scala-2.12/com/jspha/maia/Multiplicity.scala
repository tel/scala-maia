/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._
import cats.implicits._

import scala.language.higherKinds

sealed trait Multiplicity {
  type Coll[_]
  case class Wrap[A](it: Coll[A])
}

object Multiplicity {

  trait Ops[M <: Multiplicity] {
    val traversable: Traverse[M#Coll]
    def wrap[A](a: M#Coll[A]): M#Wrap[A]
  }

  sealed trait Singular extends Multiplicity {
    type Coll[A] = A
  }
  object Singular extends Singular

  implicit val SingularOps: Ops[Singular] = new Ops[Singular] {
    val traversable: Traverse[Singular#Coll] = Traverse[Singular#Coll]
    def wrap[A](a: A): Singular#Wrap[A] = Singular.Wrap(a)
  }

  sealed trait Optional extends Multiplicity {
    type Coll[A] = Option[A]
  }
  object Optional extends Optional

  implicit val OptionalOps: Ops[Optional] = new Ops[Optional] {
    val traversable: Traverse[Optional#Coll] = Traverse[Optional#Coll]
    def wrap[A](a: Option[A]): Optional#Wrap[A] = Optional.Wrap(a)
  }

  sealed trait Collection extends Multiplicity {
    type Coll[A] = List[A]
  }
  object Collection extends Collection

  implicit val CollectionOps: Ops[Collection] = new Ops[Collection] {
    val traversable: Traverse[Collection#Coll] = Traverse[Collection#Coll]
    def wrap[A](a: List[A]): Collection#Wrap[A] = Collection.Wrap(a)
  }

}
