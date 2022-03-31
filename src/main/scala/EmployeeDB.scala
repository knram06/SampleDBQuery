import scala.collection.mutable.ListBuffer

class EmployeeDB() {
  var employeeRecords: List[Employee] = List[Employee]()
  var allowedFieldNames: Array[String] = Array[String]()

  def initData(): Unit = {
    employeeRecords = List[Employee] (
      new Employee("Ram", "123", 34),
      new Employee("Joe", "124", 22),
      new Employee("Chris", "345", 27),
      new Employee("Eric", "456", 32)
    )

    // TODO: improve these field name scans
    allowedFieldNames = classOf[Employee].getDeclaredFields.map(x => x.getName)
  }

  def getData(fieldNames: List[String]): List[String] =
  {
    var fields: List[String] = List[String]()
    if (fieldNames.length == 1 && fieldNames.head == "*") {
      fields = allowedFieldNames.toList
    }
    else {
      if (!fieldNames.forall(t => allowedFieldNames.contains(t))) {
        throw new Exception
      }

      fields = fieldNames
    }

    val data = new ListBuffer[String]()
    for (rec <- employeeRecords) {
      val fieldMap = Map[String, String](
        "Name" -> rec.Name,
        "ID" -> rec.ID,
        "Age" -> rec.Age.toString
      )

      val str = new ListBuffer[String]()
      for (f <- fields) {
        fieldMap.get(f) match {
          case Some(res) => str += res
          case None => println("match not found")
        }
      }

      data += str.toList.mkString(",")
    }

    data.toList
  }
}

class Employee(var Name: String, var ID: String, var Age: Int) {
  override def toString: String =
    s"($Name, $ID, $Age)"
}
