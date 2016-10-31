
# Comms

*Typeful data fetching in Scala.js, a la GraphQL*

GraphQL demonstrates that REST-like API designs can be inefficient and 
clumsy in situations where sophisticated, rapidly changing data fetches 
are common such as in rich web and mobile applications. REST's 
presentation of data as a collection of endpoints and links obfuscates 
the often hierarchical, graphical nature of data requiring both
repeated back-and-forth transfers to traverse the graph and either 
HATEOAS or fragile encodings of API structure in all clients.

Comms, like GraphQL, represents the entire API of your application as a 
hierarchy of typed queries. It enables "bulk" queries that return a 
subgraph of the total data structure all at once and eliminate 
back-and-forth traversals.

Unlike GraphQL, Comms is designed from the ground up to work seamlessly 
within Scala's type system.
