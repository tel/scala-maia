/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._
import cats.implicits._

import scala.language.higherKinds

/**
  * [[Cardinality]]s are a feature of a collection of items. There are there
  * cardinalities: [[Cardinality.One]], [[Cardinality.Opt]], and
  * [[Cardinality.Many]] corresponding to having exactly one object, having
  * zero or one objects, and having zero, one, or many objects.
  */
sealed trait Cardinality {

  /**
    * A [[Coll]] of a given [[Cardinality]] collects as many objects as the
    * cardinality allows.
    */
  type Coll[_]
}

object Cardinality {

  /**
    * A collection of `A`s of [[Cardinality]] [[One]] is just `A`
    */
  sealed trait One extends Cardinality { type Coll[A] = A }

  /**
    * A collection of `A`s of [[Cardinality]] [[Opt]] is `Option[A]`
    */
  sealed trait Opt extends Cardinality { type Coll[A] = Option[A] }

  /**
    * A collection of `A`s of [[Cardinality]] [[Many]] is a `List[A]`
    */
  sealed trait Many extends Cardinality { type Coll[A] = List[A] }

  /**
    * [[Ops]] provide operations for working with a [[Cardinality#Coll]] of the
    * given [[Cardinality]]
    */
  trait Ops[M <: Cardinality] {
    val traversable: Traverse[M#Coll]
  }

  object Ops {
    def build[M <: Cardinality](implicit T: Traverse[M#Coll]): Ops[M] =
      new Ops[M] {
        val traversable: Traverse[M#Coll] = T
      }
  }

  implicit val SingularOps: Ops[One] = Ops.build[One]
  implicit val OptionalOps: Ops[Opt] = Ops.build[Opt]
  implicit val CollectionOps: Ops[Many] = Ops.build[Many]

}
