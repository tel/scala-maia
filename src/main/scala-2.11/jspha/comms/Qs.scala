package jspha.comms

import scala.language.higherKinds

/**
  * A type extending Qs explains how to interpret an Api specification.
  */
trait Qs {
  type Atomic[P, M <: Multiplicity, T]
  type Nested[P, M <: Multiplicity, T[_ <: Qs]]

  type Atomic1[T] = Atomic[Unit, Multiplicity.One, T]
  type Nested1[T[_ <: Qs]] = Nested[Unit, Multiplicity.One, T]

  type A[P, M <: Multiplicity, T] = Atomic[P, M, T]
  type N[P, M <: Multiplicity, T[_ <: Qs]] = Nested[P, M, T]

  type A1[T] = Atomic1[T]
  type N1[T[_ <: Qs]] = Nested1[T]
}
