# Title

ADR-001: Use types to describe APIs

# Summary

APIs will be described at the type level so as to constrain values for 
requests and responses at compile time.

# Context

The goal of this library is to make it easy to communicate between 
server and client using rich queries. These queries are defined against 
a set of query targets called an API and both the client and the server 
need to *agree* upon the API to communicate properly (or, at least, the 
server needs support a superset of the client's API).

If API checking is done at runtime it's done via functions like

    def checkReq(req: Request, api: Api): Bool
    def checkResp(resp: Response, api: Api): Bool
    
and runtime errors for `false` responses need to be intelligently 
handled. These are however *not* runtime concerns---they indicate 
improperly designed code, not a valid error state.

These functions could be used in a test, but this reveals a complex 
testing problem: how do we ensure sufficient coverage of what becomes a 
very large set of possible API requests over an expansive API? We can 
hope that exercising integration tests will create sufficient coverage, 
but this is a large maintenance burden.
 
If we use the type system to constrain requests and responses against an
API specified at the type level then we (a) get compile-time checking 
that our client and server match the API and (b) prove a universal 
property about this matching instead of a series of examples.

These two wins are strong but there is one big weakness: it's much 
harder to program a library that works at the type level and the 
resulting errors are absolutely terrible.

The core thesis of this library is that the advantages outweigh the 
costs in this circumstance.

## Prior art

- [Haskell's Servant library](https://hackage.haskell.org/package/servant)
- [Haskell's Serv library](https://github.com/tel/serv)

# Decision

We will facilitate the definition of APIs within the type system and the 
use of the type system to ensure that servers and clients cannot be 
defined out of compliance with this definition.

# Consequences

This is essentially *the* key decision underlying this library. All 
other decisions flow from here and the principle appeal of the library 
to users is judged upon this point.

The consequences for the user are significant, though. It's more 
difficult to learn a library that operates at the type level. Therefore, 
it's important for the project to eventually have good documentation and 
examples.

# Status

Accepted (2016 Dec 1)
