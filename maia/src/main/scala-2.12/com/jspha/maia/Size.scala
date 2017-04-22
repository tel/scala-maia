/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._
import cats.implicits._
import io.circe._

import scala.language.higherKinds

/**
  * [[Size]]s are a feature of a collection of items. There are there
  * cardinalities: [[One]], [[Opt]], and
  * [[Many]] corresponding to having exactly one object, having
  * zero or one objects, and having zero, one, or many objects.
  */
sealed trait Size {

  /**
    * A [[Coll]] of a given [[Size]] collects as many objects as the
    * cardinality allows.
    */
  type Coll[_]

  type Fold[IfOne <: Up, IfOpt <: Up, IfMany <: Up, Up] <: Up
}

sealed trait One extends Size {
  type Coll[A] = A
  type Fold[IfOne <: Up, IfOpt <: Up, IfMany <: Up, Up] = IfOne
}

sealed trait Opt extends Size {
  type Coll[A] = Option[A]
  type Fold[IfOne <: Up, IfOpt <: Up, IfMany <: Up, Up] = IfOpt
}

sealed trait Many extends Size {
  type Coll[A] = List[A]
  type Fold[IfOne <: Up, IfOpt <: Up, IfMany <: Up, Up] = IfMany
}

object Size {

  /**
    * [[Ops]] provide operations for working with a [[Size#Coll]] of the
    * given [[Size]]
    */
  trait Ops[M <: Size] {
    def traversable: Traverse[M#Coll]
    def encoder[A](enc: Encoder[A]): Encoder[M#Coll[A]]
    def decoder[A](dec: Decoder[A]): Decoder[M#Coll[A]]
  }

  implicit val SingularOps: Ops[One] = new Ops[One] {
    val traversable: Traverse[One#Coll] = Traverse[One#Coll]
    def encoder[A](enc: Encoder[A]): Encoder[A] = enc
    def decoder[A](dec: Decoder[A]): Decoder[A] = dec
  }

  implicit val OptionalOps: Ops[Opt] = new Ops[Opt] {
    val traversable: Traverse[Option] = Traverse[Option]

    def encoder[A](enc: Encoder[A]): Encoder[Option[A]] =
      Encoder.encodeOption(enc)

    def decoder[A](dec: Decoder[A]): Decoder[Option[A]] =
      Decoder.decodeOption(dec)
  }

  implicit val CollectionOps: Ops[Many] = new Ops[Many] {
    val traversable: Traverse[List] = Traverse[List]

    def encoder[A](enc: Encoder[A]): Encoder[List[A]] =
      Encoder.encodeFoldable(enc, implicitly)

    def decoder[A](dec: Decoder[A]): Decoder[List[A]] =
      Decoder.decodeCanBuildFrom(dec, implicitly)
  }

}
