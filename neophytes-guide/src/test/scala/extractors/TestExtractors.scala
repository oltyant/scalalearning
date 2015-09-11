package extractors

import org.scalacheck.{Gen, Properties}
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalacheck.Prop.forAll
import FlatExtractor._

/**
 * Created by oltyant on 7/19/15.
 */
class TestExtractors extends FlatSpec with GivenWhenThen {
  "The advance method" should "provide the proper scores based on its inner pattern matching" in {
    val user1 = User("John", "Doe", 10)
    val user2 = User("Jane", "Doe", 16)

    val expected = -6
    assert(expected == advance(List(user1, user2)))
  }
}

object CheckExtractors extends Properties("User") {
  import org.scalacheck.Arbitrary.arbitrary

  val genUserList: Gen[List[User]] = for {
    firstName <- arbitrary[String]
    lastName <- arbitrary[String]
    score <- arbitrary[Int]
    us <- Gen.oneOf(genUserList, Gen.const(List.empty[User]))
  } yield User(firstName, lastName, score) :: us

  property("the advance function on List of Users") = forAll(genUserList) {
    userList: List[User] => {
      if (userList.isEmpty || userList.size < 2) {
        advance(userList) == 0
      } else {
        val expected: Int = userList.take(2).sliding(2).map(e => (e.head.score, e.tail.head.score)).toList.map(p => p._1 - p._2).head
        advance(userList) == expected
      }
    }
  }
}
