/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.requestAux

import jspha.comms._

import scala.language.higherKinds

trait ToWire[Api[_ <: Spec]]
  extends (Request[Api] => WireRequest)
