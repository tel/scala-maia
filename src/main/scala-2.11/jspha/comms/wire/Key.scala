package jspha.comms.wire

import scala.language.implicitConversions
import io.circe.{Decoder, Encoder, Json}

/**
  * A Key represents one step along a path in the Api tree. Basic keys are
  * just strings (symbols), but, in general, a Key is also adorned with an
  * (untyped) argument which parameterizes the request according to the Api.
  */
case class Key(name: String, param: Json)

object Key {

  private val nullParam: Json = Encoder[Unit].apply(())

  implicit def apply(name: String): Key = apply(name, nullParam)

  def apply[P: Encoder](name: String, param: P): Key =
    apply(name, Encoder[P].apply(param))

  implicit val keyCirceEncoder: Encoder[Key] =
    Encoder[(String, Json)].contramap(k => (k.name, k.param))
  implicit val keyCirceDecoder: Decoder[Key] =
    Decoder[(String, Json)].map(p => Key(p._1, p._2))

}
