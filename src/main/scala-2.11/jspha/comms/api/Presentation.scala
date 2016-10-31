package jspha.comms.api

import jspha.comms.Api
import jspha.comms.api.Multiplicity.One

/**
  * A Presentation describes how some type is presented in the Api. Two
  * options exist: `Atomic` indicates that a particular value is available
  * for fetching as is and `Nested` indicates a sub-Api is available for
  * querying. Each presentation can be modified to take a parameter type and
  * return in some multiplicity.
  *
  * Much of the time the multiplicity is `One` and there is no interesting
  * parameterization (e.g., `Unit`), so `Atomic1` and `Nested1` type aliases
  * are provided as shortcuts for this common scenario.
  */
sealed trait Presentation

object Presentation {

  type Atomic1[T] = Atomic[Unit, One, T]
  type Nested1[A <: Api] = Nested[Unit, One, A]

  sealed trait Atomic[P, M <: Multiplicity, T] extends Presentation
  sealed trait Nested[P, M <: Multiplicity, A <: Api] extends Presentation
}

