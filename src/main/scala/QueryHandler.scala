import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object QueryHandler extends RegexParsers with PackratParsers{
  var employeeDB: EmployeeDB = new EmployeeDB()
  employeeDB.initData()

  var allowedTableNames: Seq[String] = List[String] {"employees"}
  var tableName: String = ""

  override val skipWhitespace = false

  lazy val ws: Regex = """\s+""".r

  lazy val extractedTable: PackratParser[Any] = """\w+""".r ^^ (s => parseTableName(s))

  lazy val program: PackratParser[Any] = "select" ~ ws ~ extractedFields ~ ws ~ "from" ~ ws ~ extractedTable ^^ {_ => printData(fields)}

  lazy val extractedFields: PackratParser[Any] =  words ^^ (s => fields = s)
  var fields: List[String] = List[String]()

  val words: QueryHandler.Parser[List[String]] = repsep("""\w+|\*""".r,  ",")

  def printData(fields: List[String]): Unit = {
    val data = employeeDB.getData(fields)
    for (d <- data) {
       println(d)
    }
  }

  def parseTableName(extractedTableName: String): Unit = {
    if (!allowedTableNames.contains(extractedTableName)) {
      throw new Exception
    }
    else {
      tableName = extractedTableName
      // println(tableName)
    }
  }

  def main(args: Array[String]): Unit = {
    parseAll(phrase(program), new PackratReader(new CharSequenceReader("select * from employees")))
  }
}
