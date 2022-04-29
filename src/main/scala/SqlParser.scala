import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait Node
trait SqlRelation extends Node {
  def getData: Schema
}

case class TableReln(name: String) extends SqlRelation {
  override def toString: String = name

  override def getData: Schema = {
    QueryHandler.tableManager.getTableSchema(name)
  }
}

case class Filter(field: String, op: SimpleOp, cmp: String) extends Node {
  override def toString: String = {
    s"Filter($field ${op.toString} $cmp)"
  }

  val entries: List[String] = List[String](field, op.op, cmp)
  if (!entries.forall(x => x == "") && entries.contains("")) {
    throw new Exception
  }

  def applyOnRow(inputVal: String): Boolean = {
    op.op match {
      case "lte" | "<=" => inputVal.toInt <= cmp.toInt
      case "lt"  | "<"  => inputVal.toInt < cmp.toInt
      case "gte" | ">=" => inputVal.toInt >= cmp.toInt
      case "gt"  | ">"  => inputVal.toInt > cmp.toInt
      case "eq"  | "=="  => inputVal == cmp
      case "startswith" => inputVal.startsWith(cmp)
      case "endswith"   => inputVal.endsWith(cmp)
      case _ =>
        println("Unknown operator! " + op.op)
        throw new Exception
    }
  }

  def apply(input: Schema): Schema = {
    // if field doesn't exist in Schema, just throw
    val index = input.headers.indexOf(field)
    if (index == -1) {
      println(s"Specified field: $field, does not exist in schema")
      throw new Exception
    }

    val ret = new Schema
    ret.headers ++= input.headers

    // loop through the records and apply the filter condition
    for (rec <- input.records) {
      if (applyOnRow(rec(index))) {
        ret.records.addOne(rec)
      }
    }
    ret
  }
}

case class Select(input: SqlRelation, projections: SeqProj, filter: Option[Filter]) extends SqlRelation {
  override def toString: String = {
    filter match {
      case Some(f) =>
        s"Select(${input.toString}, ${projections.toString}, ${f.toString})"
      case None =>
        s"Select(${input.toString}, ${projections.toString})"
    }
  }

  override def getData: Schema = {
    // get the input's schema
    var ret = input.getData

    // have the projection act on it - apply?
    ret = projections.apply(ret)

    // apply filter if specified
    filter match {
      case Some(f) =>
        f.apply(ret)
      case None =>
        ret
    }
  }
}

class SqlParser extends StandardTokenParsers with PackratParsers {
  class SqlLexical extends StdLexical

  override val lexical = new SqlLexical
  lexical.reserved += ("select", "from", "where", "as")
  lexical.delimiters += ("*", "+", "-", "<", "=", "<>", "!=", "<=", ">=", ">", "/", "(", ")", ",", ".", ";")

  def select: PackratParser[Select] =
    "select" ~ projections ~ relations ~ opt(filter) ^^ {case _ ~ p ~ r ~ f => Select(r, p, f)}

  def relations: PackratParser[SqlRelation] = "from" ~> simple_relation

  def simple_relation: PackratParser[SqlRelation] =
    ident ^^ (b => TableReln(b)) |
      "(" ~ select ~ ")" ^^ {case _ ~ s ~ _ => s}

  def projections: PackratParser[SeqProj] = repsep(projection, ",") ^^ {s => SeqProj(s)}
  def projection: PackratParser[SqlProj] =
    "*" ^^ {_ => StarProj()} |
      ident ~ opt("as" ~> ident) ^^ {
        case a ~ b => b match {
          case Some(s) => FieldProj(a, s)
          case None => FieldProj(a)
        }
      }

  def filter: PackratParser[Filter] = ("where" ~> ident) ~ operation ~ compare ^^ {case f ~ op ~ c => Filter(f, op, c)}
  def compare: PackratParser[String] = numericLit | stringLit

  def operation: PackratParser[SimpleOp] = ("=" | ">" | ">=" | "<" | "<=") ^^ { s => SimpleOp(s) }

  def parse(sql: String): Select = {
    val parseResult = phrase(select)(new lexical.Scanner(sql))
    parseResult.get
  }
}
