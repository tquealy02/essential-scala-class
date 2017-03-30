//full disclosure I didn't know anything about JSON so I had to
// look at answer to even know what JSON was

sealed trait Json {
  def print: String

}
final case class JsonString(value: String) extends Json {
  def print = s"'$value'"
}
final case class JsonDouble(value: Double) extends Json {
  def print = value.toString
}
final case class JsonBoolean(value: Boolean) extends Json {
  def print = value.toString
}
// todo: add all basic types to this


sealed trait JsonListLike extends Json {
  // note a huge fan of this as the end's done need this - could probably do
  // different type matching to get around this
  def seqPrint: String
}
sealed trait JsonSequence extends JsonListLike {

}

//one note I believe this allows for nested sequences which I don't think the solution
// in the book does
final case class JsonListCell(head: Json, tail: JsonSequence) extends JsonSequence {
  def print = s"[ $seqPrint ]"

  def seqPrint = tail match {
    case JsonListEnd => head.print
    case _ => s"${head.print}, ${tail.seqPrint}"


  }
}

  final case object JsonListEnd extends JsonSequence {
    def print = ""

    def seqPrint = ""
  }

  // here is where I really needed to cheat as I didn't know the object structure
  sealed trait JsonObj extends JsonListLike

  final case class jsonObjItem(key: String, value: Json, tail: JsonObj) extends JsonObj {
    def print = s"{ $seqPrint }"

    def seqPrint = tail match {
      case JsonObjEnd => s" '${key}' : ${value.print}"
      case _ => s"'${key}' : ${value.print}, ${tail.seqPrint}"


    }
  }


final case object JsonObjEnd extends JsonObj {

  def print = ""
  def seqPrint = ""
}


val foo1 = JsonListCell(JsonDouble(4.0),JsonListCell(JsonString("hello"),JsonListEnd))
foo1.print

val foo2 = JsonListCell(JsonString("a string"), JsonListCell(JsonDouble(1.0), JsonListCell(JsonBoolean(true), JsonListEnd)))
foo2.print

//nesting!
val foo3 = JsonListCell(foo1,foo2)
foo3.print



