package jspha.comms

import scala.language.higherKinds
import io.circe.{Decoder, Encoder}

package object qs {

  type Request[Api[_ <: Qs]] = Api[ReqS]
  type Response[Api[_ <: Qs]] = Api[RespS]

  implicit def ResponseEncoder[Api[_ <: Qs]]: Encoder[Response[Api]] =
    ???

  implicit def ResponseDecoder[Api[_ <: Qs]]: Decoder[Response[Api]] =
    ???

  implicit def RequestEncoder[Api[_ <: Qs]: ReqS.ToWire]: Encoder[Request[Api]] =
    Encoder[wire.Request].contramap(ReqS.toWire[Api])

}
