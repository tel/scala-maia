package jspha.comms.test

import shapeless._
import jspha.comms._
import jspha.comms.specs.Fetch
import scala.language.higherKinds
import utest._

object ClientExample extends TestSuite {

  case class Loc[S <: Qs](
      lat: S#A1[Double],
      lon: S#A1[Double]
  )
  case class City[S <: Qs](
      name: S#A1[String],
      loc: S#N1[Loc],
      mayor: S#N1[User]
  )
  case class User[S <: Qs](
      home: S#N1[City],
      loc: S#N1[Loc],
      name: S#A1[String]
  )
  case class Query[S <: Qs](
      currentUser: S#N1[User]
  )

  val qFetch: Query[Fetch] =
    the[Fetch.Auto[Query[Fetch]]].value

  val lookup = qFetch.currentUser { u =>
    import cats.syntax.cartesian._

    val lLoc = u.loc { l =>
      (l.lat.get |@| l.lon.get).tupled
    }

    (u.name.get |@| lLoc).tupled
  }

  val tests = this {

    'qFetch {
      * - { qFetch.currentUser.name ==> "currentUser" }
    }

  }

}
