/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

/**
  * In order to not require passing parameters to every query we allow for
  * defaults to be specified. These must be available and identical on client
  * and server, so it's best to be careful in using them.
  */
trait Default[T] {
  val value: T
}

object Default {

  def apply[T](t: T): Default[T] = new Default[T] {
    val value: T = t
  }

  implicit val DefaultParamUnit: Default[Unit] =
    apply(())

  implicit def DefaultParamOption[A]: Default[Option[A]] =
    apply(None)

}

