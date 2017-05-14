/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.serialization

import cats.Monad
import com.jspha.maia._

trait SerializationOps[P <: SerializationParams] {
  val obj: Section[P, Map[String, P#Target]]
  val params: Section[P, Vector[(P#Target, P#Target)]]
  val ofOne: Section[P, One#Coll[P#Target]]
  val ofOpt: Section[P, Opt#Coll[P#Target]]
  val ofMany: Section[P, Many#Coll[P#Target]]

  val unit: Section[P, Unit]

  implicit val injectMonad: Monad[P#InjectEff]
  implicit val retractMonad: Monad[P#RetractEff]
}
