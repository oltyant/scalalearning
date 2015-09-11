package extractors

/**
 * Created by root on 7/19/15.
 */
package object FlatExtractor {
  case class User(firstName: String, lastName: String, score: Int = 0)
  def advance(xs: List[User]): Int = xs match {
    case User(_, _, score1) :: User(_, _, score2) :: _ => score1 - score2
    case _ => 0
  }
}

package manual {
  trait User {
    def name: String
  }
  class FreeUser(val name: String) extends User
  class PremiumUser(val name: String) extends User

  object FreeUser {
    def unapply(u: User): Option[String] = {
      Some(u.name)
    }
  }
  object PremiumUser {
    def unapply(u: User): Option[String] = {
      Some(u.name)
    }
  }
}
