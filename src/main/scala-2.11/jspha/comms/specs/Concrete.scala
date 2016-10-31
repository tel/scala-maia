package jspha.comms.specs

import scala.language.higherKinds
import jspha.comms._

/**
  * Concrete extends Spec where all atomic queries are materialized.
  */
trait Concrete extends Qs {
  type Atomic[P, M <: Multiplicity, T] = P => M#Apply[T]
  type Nested[P, M <: Multiplicity, T[_ <: Qs]] = Unit
}

object Concrete extends Concrete
