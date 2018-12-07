package typeclasses.examples

/**
  * Created by oltyan on 2017.06.04..
  */
sealed trait Json
final case class JsonObject(value: Map[String, Json]) extends Json
final case class JsonString(value: String) extends Json
final case class JsonNumber[T : Numeric](value: T) extends Json
final case class JsonBoolean(value: Boolean) extends Json
final case class JsonArray(list: Array[Json]) extends Json

trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonInstances {
  implicit object stringJsonWriter extends JsonWriter[String] {
    override def write(value: String) = JsonString(value)
  }
  implicit object booleanJsonWriter extends JsonWriter[Boolean] {
    override def write(value: Boolean) = JsonBoolean(value)
  }
  implicit object intJsonWriter extends  JsonWriter[Int] {
    override def write(value: Int) = JsonNumber(value)
  }
  implicit object longJsonWriter extends JsonWriter[Long] {
    override def write(value: Long) = JsonNumber(value)
  }
  implicit object doubleJsonWriter extends JsonWriter[Double] {
    override def write(value: Double) = JsonNumber(value)
  }
  implicit object floatJsonWriter extends JsonWriter[Float] {
    override def write(value: Float) = JsonNumber(value)
  }
  implicit object personJsonWriter extends JsonWriter[Person] {
    override def write(value: Person) =
      JsonObject(
        Map(
          "name" -> JsonString(value.name),
          "id" -> JsonNumber(value.id),
          "email" -> JsonString(value.email)
        )
      )
  }
}

object JsonInterface {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

object JsonSyntax {
  implicit class JsonWriteOps[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]) = writer.write(value)
  }
}
