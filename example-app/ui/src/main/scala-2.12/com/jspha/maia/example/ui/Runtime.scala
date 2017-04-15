/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.example.ui

import scala.scalajs.js.JSApp

import fr.hmil.roshttp.HttpRequest
import fr.hmil.roshttp.Protocol.HTTP

object Runtime extends JSApp {

  val request: HttpRequest =
    HttpRequest()
      .withProtocol(HTTP)
      .withHost("localhost")
      .withPort(8080)
      .withPath("/api")

  def main(): Unit = {
    println("Loaded!!!")
    App.run(request)
  }
}
