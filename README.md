
# Maia ![Maia logo](../master/docs/logos/maia-63x40.png?raw=true)

*Highly-typeful data fetching in Scala.js, a la GraphQL*

[![Build
Status](https://travis-ci.org/tel/scala-maia.svg?branch=master)](https://travis-ci.org/tel/scala-maia)

## Welcome

Maia is a Scala library for the JVM and Javascript platforms that enables
application authors to share structured data between processes using a
well-typed query language. It shares similar goals with the larger [GraphQL
project](http://graphql.org/) but is optimized for the Scala platform and
ecosystem and offers more type-level guarantees to users for that restriction.

## Etymology

Maia is Greek for "midwife" and name of the eldest of the Pleiades, but more
directly it refers to the maieutic, or socratic, method---a didactic method of
having a discussion through a series of questions and answers.

## Contributing

Maia is intended to follow a [C4-like](https://rfc.zeromq.org/spec:42/C4/)
contribution system. As Maia is still in the early stages of design this is not
yet in place, but contributions are still welcome.

Maintainers are to merge all _correct_ patches which _aid in the discovery or
implementation of the Maia API_ and therefore value judgements can be made by
the maintainers as to whether a patch is moving in a valuable direction at this
time.

### Patch guidelines

- Patches must pass continuous integration on the Travis CI instance associated
  with this Github repository.
- Code must be formatted according to the ScalaFmt configuration provided.
- Patches must pass test suite which includes WartRemover and ScalaStyle lints,
  e.g.
  - Tabs are disallowed
  - Whitespace at end of lines is disallowed
  - MPLv2.0 license must be the header of every file
  - Public methods must be given an explicit type
  - Type-unsafe `==` is disallowed
  - `return`, `null`, `.clone()`, and `.finalize()` are disallowed
  - Structural types are disallowed
  - &c.
    - For more, see [Wartremover](https://github.com/wartremover/wartremover)
      and `scalastyle-config.xml`

### Maintainers

- Joseph Abrahamson <me@jspha.com>

## License, generally

Code should be licensed under the Mozilla Public License Version 2.0.  The MPL
operates on a file-by-file basis. Commits adding new files should include the
following MPL license header at the top of the file

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.

