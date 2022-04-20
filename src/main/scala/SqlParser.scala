import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait Node

trait SqlProj extends Node {
  def toString: String
}

case class StarProj() extends SqlProj {
  override def toString = "*"
}

case class FieldProj(field: String, alias: String = null) extends SqlProj() {
  override def toString: String = {
    if (alias != null) {
      s"FieldProj($field, $alias)"
    }
    else {
      s"FieldProj($field)"
    }
  }
}

trait SqlRelation extends Node
case class TableReln(name: String) extends SqlRelation {
  override def toString: String = name
}


trait Operation extends Node
case class SimpleOp(op: String) extends Operation {
  override def toString: String = op
}

case class Filter(field: String, op: SimpleOp, cmp: String) extends Node {
  override def toString: String = {
    s"Filter($field ${op.toString} $cmp)"
  }

  val entries: List[String] = List[String](field, op.op, cmp)
  if (!entries.forall(x => x == "") && entries.contains("")) {
    throw new Exception
  }

  def apply(inputField: String, inputVal: String): Boolean = {
    // if we are not at the field to match
    // return true
    if (field != inputField) {
      return true
    }

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
}

case class Select(projections: Seq[SqlProj], table: TableReln, filter: Option[Filter]) extends Node {
  override def toString: String = {
    filter match {
      case Some(f) =>
        s"Select(${projections.mkString(",")}, ${table.toString}, ${f.toString})"
      case None =>
        s"Select(${projections.mkString(",")}, ${table.toString})"
    }
  }
}

class SqlParser extends StandardTokenParsers with PackratParsers {
  class SqlLexical extends StdLexical

  override val lexical = new SqlLexical
  lexical.reserved += ("select", "from", "where", "as")
  lexical.delimiters += ("*", "+", "-", "<", "=", "<>", "!=", "<=", ">=", ">", "/", "(", ")", ",", ".", ";")

  def select: PackratParser[Select] =
    "select" ~ projections ~ table ~ opt(filter) ^^ {case _ ~ p ~ t ~ f => Select(p, t, f)}

  def projections: PackratParser[Seq[SqlProj]] = repsep(projection, ",")
  def projection: PackratParser[SqlProj] =
    "*" ^^ {_ => StarProj()} |
      ident ~ opt("as" ~> ident) ^^ {
        case a ~ b => b match {
          case Some(s) => FieldProj(a, s)
          case None => FieldProj(a)
        }
      }

  def table: PackratParser[TableReln] = ("from" ~> ident) ^^ (b => TableReln(b))

  def filter: PackratParser[Filter] = ("where" ~> ident) ~ operation ~ compare ^^ {case f ~ op ~ c => Filter(f, op, c)}
  def compare: PackratParser[String] = numericLit | stringLit

  def operation: PackratParser[SimpleOp] = ("=" | ">" | ">=" | "<" | "<=") ^^ { s => SimpleOp(s) }

  def parse(sql: String): Select = {
    val parseResult = phrase(select)(new lexical.Scanner(sql))
    parseResult.get
  }
}
