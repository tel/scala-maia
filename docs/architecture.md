
# Maia Architecture

## Overview

Maia facilitates a client querying a server and the server responding to it.
Users of Maia implement the query definitions on the client side, give meaning
to them in server code, and usher the query requests and their responses from
client to server. Maia provides tools for defining the "API" which queries are
written against and provides boilerplate interpretation and serialization code.

More concretely, with Maia you perform the following steps:

- Define a series of types which describe your request-response API
- Implement code which produces data to substantiate each "path" in the API
  - Maia turns this into a "request handler"
- Use Maia's query-construction toolkit to build API requests in the client
  - Requests can be "bundled" to reduce bandwidth and enable request definition
    to be distributed throughout the client
- Pass the client's requests to Maia's request handler to receive responses
  - Responses are also bundled and can be cached forming a "database" on the
    client
- Convert bulk and aggregate responses into the particular data the client
  requests needed
  - In other words, unbundle the aggregate responses to get the data particular
    to each unbundled request

## API types

Key to understanding Maia is understanding the way in which API types are
defined. Maia asks you to define a "graph-like" data type which defines your API
and then uses the structure of this type to provide boilerplate and ensure
requests and responses match.

An API type is a type parameterized by a `Mode` where every field takes a type
from that `Mode`. This allows us to use the "blueprints" of these types in many
different forms while ensuring that every type lines up perfectly. Maia iterates
over these blueprints at compile time to generate large amounts of boilerplate
necessary for operating atop your API.

A simple example API follows:

```scala
final case class Person[M <: Mode](
  name: M#Atom[String],
  city: M#Obj[City]
)

final case class City[M <: Mode](
  name: M#Atom[String],
  mayor: M#Obj[Person]
)
```

Each of these case classes is parameterized by a choice of `Mode` and has only
fields at types defined by the `Mode`, `M`. `Person` and `City` reference one
another in a loop and, depending on the semantics of the API you're offering,
either one could be the "top-level" of your API.

### API mode synonyms

Instead of writing `City[RequestMode]` all over the place, Maia defines a type
alias `type Request[Api[_ <: Mode]] = Api[RequestMode]`. These make reading API
types much more convenient, but they're really only there for convenience.
