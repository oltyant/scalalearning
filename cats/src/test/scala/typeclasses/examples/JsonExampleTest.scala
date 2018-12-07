package typeclasses.examples

import org.scalatest._
import typeclasses.examples.JsonInstances._

/**
  * Created by oltyan on 2017.06.05.
  */
class JsonExampleTest extends WordSpec with Matchers {
  val me = Person("Programmer", 1234677L, "programmer@email.com")
  val result = JsonObject(Map("name" -> JsonString(me.name), "id" -> JsonNumber(me.id), "email" -> JsonString(me.email)))

  "'JsonInterface' type class interface" should {
    "convert the given Person to JsonObject" in {
      val jsonMe = JsonInterface.toJson(me)
      assert(jsonMe == result)
    }
  }

  "'JsonSyntax' type class syntax" should {
    "convert the given Person to JsonObject" in {
      import typeclasses.examples.JsonSyntax._
      val jsonMe = me.toJson
      assert(jsonMe == result)
    }
  }
}
