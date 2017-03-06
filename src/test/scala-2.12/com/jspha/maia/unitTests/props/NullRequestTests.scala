/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.unitTests.props

import com.jspha.maia._
import com.jspha.maia.exampleApi._
import com.jspha.maia.props.NullRequest
import com.jspha.maia.props.NullRequest.Worker
import shapeless._
import shapeless.labelled._

object NullRequestTests {

  val kx = Witness('x)
  val ky = Witness('y)

  val workerHNil: Worker[HNil] = implicitly
  val workerHConsAtom: Worker[
    FieldType[kx.T, RequestMode.IndexedAtom[Unit, Int]] ::
      HNil] =
    implicitly
  val workerHConsAtom2: Worker[
    FieldType[ky.T, RequestMode.IndexedAtom[Unit, Int]] ::
      FieldType[kx.T, RequestMode.IndexedAtom[Unit, Int]] ::
        HNil] =
    implicitly
  val workerHConsObj: Worker[
    FieldType[kx.T, RequestMode.Obj[Location]] :: HNil] =
    implicitly

  val location: Request[Location] = implicitly[NullRequest[Location]].request
  val user: Request[User] = implicitly[NullRequest[User]].request
  val city: Request[City] = implicitly[NullRequest[City]].request
  val api: Request[Api] = implicitly[NullRequest[Api]].request

}
