/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.example.ui

import com.jspha.maia._
import com.jspha.maia.example.api._

object Lookups {

  val tlq: QueriesAt[TopLevel] =
    typelevel.GetQueriesAt[TopLevel]

  val lookupCount: Query[TopLevel, Nothing, Int] =
    tlq.getCount

}
