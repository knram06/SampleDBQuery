import scala.collection.mutable.ListBuffer

trait TableRecord {
  def allowedFieldNames: Seq[String]
  def toListStr: List[String]
}

class Table(name: String) extends Node {
  val headers = new ListBuffer[String]
  val records = new ListBuffer[TableRecord]

  def addList(entries: Seq[TableRecord]): Any = {
    records ++= entries
    headers ++= getAllowedFieldNames
  }

  def getTableName: String = name

  def getAllowedFieldNames: Seq[String] = {
    records.head.allowedFieldNames
  }

  def getSchema: Schema = {
    val schema = new Schema()
    schema.setHeaders(headers.toList)

    // now add the records
    for (rec <- records) {
      schema.addEntry(rec.toListStr)
    }

    schema
  }

  // returns header + records
  def getData: List[List[String]] = {
    val ret = new ListBuffer[List[String]]

    // add the header
    ret += headers.toList

    // now add the records
    for (rec <- records) {
      ret += rec.toListStr
    }

    ret.toList
  }
}
