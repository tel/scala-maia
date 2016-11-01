package jspha.comms.qs

import scala.language.higherKinds
import jspha.comms._

trait RespS extends Qs {
  type Atomic[P, M <: Mult, T] =
    Map[P, M#Apply[T]]
  type Nested[P, M <: Mult, T[_ <: Qs]] =
    Map[P, M#Apply[Response[T]]]
}

