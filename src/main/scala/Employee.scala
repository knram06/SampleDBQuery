import scala.collection.mutable.ListBuffer

class Employee(Name: String, ID: String, Age: Int) extends TableRecord {
  override def toString: String =
    s"($Name, $ID, $Age)"

  override def toListStr: List[String] = {
    val buf = new ListBuffer[String]
    buf += Name
    buf += ID
    buf += Age.toString

    buf.toList
  }

  override def allowedFieldNames: Seq[String] = {
    Array("Name", "ID", "Age")
  }
}