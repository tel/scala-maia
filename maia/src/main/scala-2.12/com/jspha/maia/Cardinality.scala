/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import cats._
import cats.implicits._
import io.circe._

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
