/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.example.ui

import com.jspha.maia._
import com.jspha.maia.example.api._
import com.jspha.maia.generic.HasQuery

object Lookups {

  val tlq: Query[TopLevel] =
    implicitly[HasQuery[TopLevel]].query

  val lookupCount: LookupS[TopLevel, Int] =
    tlq.getCount

}
