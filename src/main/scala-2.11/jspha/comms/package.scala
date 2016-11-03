/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha

import scala.language.higherKinds

package object comms {

  type Request[Api[_ <: Spec]] = Api[ReqSpec]
  type Response[Api[_ <: Spec]] = Api[RespSpec]

}
