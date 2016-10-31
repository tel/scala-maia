package jspha.comms

import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.{TypeTag, typeOf}

/**
  * A Dyn value is a dynamically typed value which carries along with it a
  * TypeTag.
  */
sealed trait Dyn {

  /**
    * Attempts to unseal a dynamically typed value. If the runtime type is a
    * subtype of `A` then we can successfully unlock `Some` value. Otherwise,
    * `None`.
    */
  def unseal[A: TypeTag]: Option[Dyn.Box[A]]

  /**
    * Unseal and then open the box.
    */
  def open[A: TypeTag]: Option[A] =
    unseal.map(box => box.value)
}

object Dyn {

  /**
    * A value adorned with its TypeTag. A Dyn value is a Box that has
    * forgotten its type.
    */
  sealed trait Box[A] {
    def seal: Dyn = Seal(this)
    val value: A
    val typeTag: TypeTag[A]
  }

  object Box {
    def apply[A](a: A)(implicit ty: TypeTag[A]): Box[A] = Mk(a, ty)

    private case class Mk[A](value: A, typeTag: TypeTag[A]) extends Box[A]

    /**
      * Type tags are forgotten on serialization.
      */
    implicit def boxCirceEncoder[A: Encoder]: Encoder[Box[A]] =
      Encoder[A].contramap(box => box.value)

    /**
      * If a value can be successfully read as a type then it will be
      * packaged into the Box as that type. This means that it's possible to
      * lie with dynamic types!
      */
    implicit def boxCirceDecoder[A: Decoder: TypeTag]: Decoder[Box[A]] =
      Decoder[A].map(a => Box(a))
  }

  case class Seal[A](box: Box[A]) extends Dyn {

    def unseal[B: TypeTag]: Option[Box[B]] =
      if (box.typeTag.tpe <:< typeOf[B])
        // This coercion is OK; we've proven type subsumption even if we can't
        // say as much
        Some(box.asInstanceOf)
      else
        None

  }

  def apply[A: TypeTag](a: A): Dyn = Seal(Box(a))

}
