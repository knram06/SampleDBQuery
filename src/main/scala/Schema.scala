import scala.collection.mutable.ListBuffer

class Schema {
  val headers = new ListBuffer[String]
  val records = new ListBuffer[List[String]]

  def addEntry(entry: List[String]): Any = {
    records += entry
  }

  def addList(entries: List[List[String]]): Any = {
    records ++= entries
  }

  def setHeaders(input: List[String]): Unit = {
    headers ++= input
  }

  def getHeaders: Seq[String] = {
    headers.toList
  }

  // returns header + records
  def getData: List[List[String]] = {
    val ret = new ListBuffer[List[String]]

    // add the header
    ret += headers.toList
    ret ++= records.toList

    ret.toList
  }

  override def toString: String = {
    var ret = ""

    ret += headers.mkString(",")
    ret += "\n"
    for (rec <- records) {
      ret += rec.mkString(",")
      ret += "\n"
    }

    ret
  }
}
