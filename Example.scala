
// Given an interface like

sealed trait Mult
sealed trait One extends Mult
sealed trait ZeroOrOne extends Mult
sealed trait Many extends Mult

case class NoParam()

trait Qs {
  type Atomic[P, M <: Mult, T]
  type Nested[P, M <: Mult, T[_ <: Qs]]

  final type A1[T] = Atomic[NoParam, One, T]
  final type N1[T[_ <: Qs]] = Nested[NoParam, One, T]
}

// We build an API like

// Semantic type wrapper. Needs instances for Circe KeyEncoder, KeyDecoder,
// Encoder, and Decoder.
case class Name(name: String)

// Simple data types have only "atomic" fields.
case class Loc[S <: Qs](
  lat: S#A1[Double],
  lon: S#A1[Double]
)

// Complex data types also nest inner queries.
case class Person[S <: Qs](
  name: S#A1[Name],
  hometown: S#N1[City],
)

// Data types can query one another recursively.
case class City[S <: Qs](
  name: S#A1[Name],
  location: S#N1[Loc],
  mayor: S#Nested[NoParam, ZeroOrOne, Person]
)

// Queries can also be parameterized and may return in various multiplicities.
case class MasterQuery[S <: Qs](
  // People have unique names!
  getPerson: S#Nested[Name, ZeroOrOne, Person],
  getAllPeople: S#Nested[NoParam, Many, Person]
)

// --- --- --- ---

// Assume a trait something like

trait Endpoint {
  type Query[_ <: Qs]

  def buildQuery[T](k: QueryBuilder[Api] => Lookup[Api, T]): Lookup[Api, T]
}

// where Lookup handles building a query upon a given Api and handling a
// response from the server to that Query

case class Lookup[Api, T](
  request: Request[Api],
  handleResponse: Response[Api] => Option[T]
)

type Request[Api[_ <: Qs]] = Api[ReqS]
type Response[Api[_ <: Qs]] = Api[RespS]

trait ReqS extends Qs {
  type Atomic[P, M <: Mult, T] = Set[P]
  type Nested[P, M <: Mult, T[_ <: Qs]] = HashMap[P, Request[T]]
}

// We build Lookup[Api, T] values like

val lookup: Lookup[(Double, Double)] =
  Ep.buildQuery { (q: QueryBuilder[MasterQuery]) =>

    q.getPerson.apply(Name("Joseph Abrahamson")) { person =>
      // Anonymous function inference actually works fine here

      person.hometown.get { city =>
        city.location.get { loc =>
          (loc.lat.get |@| loc.lon.get).tupled
        }
      }

    }

}

// Then using functions like

def sendToServer[Api[_ <: Qs]](req: Request[Api]): Task[Response[Api]]

// we collect all the needed data and have responses available to convert to
// userland values via the `.handleResponse` member on `lookup` above.
