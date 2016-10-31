package jspha.comms

import io.circe.Decoder.Result
import io.circe._

import scala.collection.immutable.HashMap

/**
  * A Comms wire request is a collection of paths along an Api. Since paths
  * often have common prefixes these are compressed into a tree.
  *
  * Requests are completely untyped which means that they can have multiple
  * interpretations under different Apis. In practice, this simply means that
  * in order to parse a request we need an Api.
  */
case class Request(here: HashMap[Key.Dyn, Request]) {

  def nest(key: Key.Dyn): Request =
    Request(key -> this)

  def toPairs: List[(Key.Dyn, Request)] =
    here.toList

  def ++(other: Request): Request =
    Request(
      here.merged(other.here) {
        case ((k1, v1), (k2, v2)) => (k1, v1 ++ v2)
      }
    )

}

object Request {

  def apply(pairs: (Key.Dyn, Request)*): Request =
    ofPairs(pairs)

  def ofPairs(pairs: Seq[(Key.Dyn, Request)]): Request =
    Request(HashMap(pairs: _*))

  /**
    * The unit request requests a single symbol at the root.
    */
  def unit(key: Key.Dyn): Request =
    Request(HashMap(key -> empty))

  val empty: Request =
    Request()

  private object CirceHelper {
    def lazyEncoder[A](e: => Encoder[A]): Encoder[A] =
      new Encoder[A] {
        lazy val delegate = e
        def apply(a: A): Json = delegate(a)
      }

    def lazyDecoder[A](e: => Decoder[A]): Decoder[A] =
      new Decoder[A] {
        lazy val delegate = e
        def apply(c: HCursor): Result[A] = delegate(c)
      }
  }

  implicit lazy val requestCirceEncoder: Encoder[Request] =
    CirceHelper.lazyEncoder(new Encoder[Request] {
      def apply(a: Request): Json =
        Encoder[List[(Key.Dyn, Request)]].apply(a.toPairs)
    })

}
