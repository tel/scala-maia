package jspha.comms

import cats.data.Xor

import scala.collection.immutable.HashMap

/**
  * A Comms wire response is a subtree of an Api with values adorned on the
  * leaves.
  *
  * Due to the types forgotten inside of a Response it's not possible to
  * serialize them directly. Instead, they must be serialized against an Api.
  */
case class Response(here: HashMap[Key, MSet[Dyn] Xor MSet[Response]]) {

  def apply(key: Key): Option[MSet[Dyn] Xor MSet[Response]] =
    here.get(key)

  def nest(key: Key): Response =
    Response(key -> Xor.right(MSet.one(this)))

}

object Response {

  def apply(pairs: (Key, MSet[Dyn] Xor MSet[Response])*): Response =
    Response(HashMap(pairs:_*))

}
