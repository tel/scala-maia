package jspha.comms

import io.circe.{Decoder, Encoder, Json}

import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.language.implicitConversions

/**
  * A Key represents one step along a path in the Api tree. Basic keys are
  * just strings (symbols), but, in general, a Key is also adorned with an
  * (untyped) argument which parameterizes the request according to the Api.
  */
case class Key[P](name: String, param: P) {

  def forget(implicit typeTag: TypeTag[P],
             encoder: Encoder[P]): Key.Dyn =
    Key.Dyn.Mk(this)(encoder, typeTag)

}

object Key {

  sealed trait Dyn {
    val encoded: Json
    def project[Q: TypeTag]: Option[Key[Q]]
  }

  object Dyn {

    def apply[P: Encoder : TypeTag](name: String, param: P) =
      Mk(Key(name, param))

    case class Mk[P](key: Key[P])(implicit encoder: Encoder[P],
      typeTag: TypeTag[P])
      extends Dyn {

      def project[Q: TypeTag]: Option[Key[Q]] =
        if (typeTag.tpe <:< typeOf[Q])
          Some(key.asInstanceOf)
        else
          None

      lazy val encoded: Json =
        keyEncoder(encoder)(key)

    }

    implicit val dynamicKeyEncoder: Encoder[Dyn] = new Encoder[Dyn] {
      def apply(a: Dyn): Json = a.encoded
    }

    // Cannot be implicit since it really depends upon the (otherwise known)
    // value of A.
    def dynamicKeyDecoder[A: Encoder: Decoder: TypeTag]: Decoder[Dyn] =
      keyDecoder(Decoder[A]).map(_.forget)

  }

  implicit def keyEncoder[P: Encoder]: Encoder[Key[P]] =
    Encoder[(String, P)].contramap(k => (k.name, k.param))

  implicit def keyDecoder[P: Decoder]: Decoder[Key[P]] =
    Decoder[(String, P)].map(p => Key(p._1, p._2))

}
