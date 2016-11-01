package jspha.comms

import scala.language.higherKinds

trait Qs {
  type Atomic[P, M <: Mult, T]
  type Nested[P, M <: Mult, T[_ <: Qs]]

  final type Atomic1[T] = Atomic[Unit, Mult.One, T]
  final type Nested1[T[_ <: Qs]] = Nested[Unit, Mult.One, T]

  final type A[P, M <: Mult, T] = Atomic[P, M, T]
  final type N[P, M <: Mult, T[_ <: Qs]] = Nested[P, M, T]

  final type A1[T] = Atomic1[T]
  final type N1[T[_ <: Qs]] = Nested1[T]
}
