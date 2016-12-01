# Title

ADR-002: Use higher-kinded, parameterized types for API specifications

# Summary

API specifications will be types that look a bit like 

    case class Person[S <: Spec](
      name: S#Atomic[Int],
      mother: S#Nested[Person]
    )
    
so that they can take different forms for request or response via 
different parameterizations.

# Context

The principle nature of the type of an API specification is that it 
should provide the skeletal structure to describe three things

- a builder of queries against the API,
- the type of "subgraphs" of the API which serve as valid server 
  responses, and
- the type of server handlers which can be composed together to create 
  those responses.
  
Many options exist here and most require working through a generic 
representation via Shapeless, but it's easy for these to present a lot 
of complexity to the user.

By parameterizing an API description by an unknown "spec" we can provide 
whatever definitions are convenient for the types of the values and 
therefore have the same "skeleton" take all three forms.

# Decision

Support using the `Spec`-parameterized case class format for describing 
APIs.

# Status

Accepted (2016 Dec 1)
