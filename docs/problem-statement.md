# Problem Statement

Client-Server data fetching is sophisticated and quickly changing. Often 
bad team dynamics are caused when client product evolution is dragged by
slower server API updates. This problem is exacerbated by having 
multiple clients each requesting unique views of the data.

## Context

Today rich internet applications (clients) often request the data they 
need over a REST-like API. This is essentially a flat list of 
parameterized requests (endpoints), each of which returning data in a 
format that's essentially custom to that endpoint. Good design 
suggests that these endpoints should be standardized, idempotent, 
orthogonal to client needs, and use a common language for results. In 
practice, this rarely occurs.

Instead, in any rapidly growing application the client needs are often 
specific and complicated sets of queries. Oftentimes, these queries 
cannot be issued all at once but instead must be sent in waves as the 
results from earlier waves impact the design of later waves.

API designers find it difficult to design an API which is truly 
orthogonal. Such an API results in significant rework and exacerbates
the need for using waves of client queries.
