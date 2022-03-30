import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object TestPackratParser extends RegexParsers with PackratParsers {

  override val skipWhitespace = false

  lazy val ws: Regex = """\s+""".r

  lazy val program: PackratParser[Any] = "start" ~ ws ~ water ~ ws ~ "end" ^^ (_ => println("program"))

  lazy val water: PackratParser[Any] =  words ^^ (_ => println("water"))

  val words: TestPackratParser.Parser[List[String]] = repsep("""\w+""".r,  ws ~ not("end") ^^ (_ => ""))

  def main(args: Array[String]): Unit = {
    val employeeRecords: List[Employee] = List(
      new Employee("Joe", "123", 33),
      new Employee("Ram", "124", 34),
      new Employee("Sam", "345", 23),
      new Employee("Kris", "346", 27),
    )

    parseAll(phrase(program), new PackratReader(new CharSequenceReader("start something here end")))
  }
}
