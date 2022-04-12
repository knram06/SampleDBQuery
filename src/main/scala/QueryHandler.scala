import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object QueryHandler extends RegexParsers with PackratParsers{
  var employeeDB = new EmployeeDB()
  employeeDB.initData()

  var allowedTableNames: Seq[String] = List[String] {"employees"}

  override val skipWhitespace = false

  // for filtering conditions
  lazy val filterFieldRegex: PackratParser[Unit] = """\w+""".r ^^ (s => filterField = s)
  var filterField = ""

  lazy val filterOpRegex: PackratParser[Unit] = """[a-zA-Z0-9<>=!]+""".r ^^ (s => filterOp = s)
  var filterOp = ""

  lazy val filterCondRegex: PackratParser[Unit] = """\w+""".r ^^ (s => filterCond = s)
  var filterCond = ""

  lazy val filterQuery: PackratParser[Any] = "where" ~ ws ~ filterFieldRegex ~ ws ~ filterOpRegex ~ ws ~ filterCondRegex

  lazy val ws: Regex = """\s+""".r

  var tableName: String = ""
  lazy val extractedTable: PackratParser[Any] = """\w+""".r ^^ (s => parseTableName(s))

  lazy val program: PackratParser[Any] = "select" ~ ws ~ extractedFields ~ ws ~ "from" ~ ws ~ extractedTable ~ ws ~ filterQuery ^^ { _ => printData(fields, new FilterQuery(filterField, filterOp, filterCond))}

  lazy val extractedFields: PackratParser[Any] =  words ^^ (s => fields = s)
  var fields: List[String] = List[String]()

  val words: QueryHandler.Parser[List[String]] = repsep("""\w+|\*""".r,  ",")

  def printData(fields: List[String], filterObj: FilterQuery): Unit = {
    val data = employeeDB.getData(fields, filterObj)
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
    }
  }

  def main(args: Array[String]): Unit = {
    parseAll(phrase(program), new PackratReader(new CharSequenceReader("select * from employees where Age <= 30")))
  }
}
