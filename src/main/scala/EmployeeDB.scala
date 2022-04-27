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

    // use reflection to get the properties names
    allowedFieldNames = classOf[Employee].getDeclaredFields.map(x => x.getName)
  }

  def getData(fieldNames: List[String], filterObj: Option[Filter]): List[String] =
  {
    val data = new ListBuffer[String]()
    for (rec <- employeeRecords) {
      val fieldMap = Map[String, String](
        "Name" -> rec.Name,
        "ID" -> rec.ID,
        "Age" -> rec.Age.toString
      )

      val str = new ListBuffer[String]()
      for (f <- fieldNames) {
        fieldMap.get(f) match {
          case Some(res) =>
            filterObj match {
              case Some(filt) =>
                if (filt.apply (f, res) ) {
                  str += res
                }
              case None => str += res
            }
          case None => println("match not found")
        }
      }

      // if filter condition was true for every field
      // only then add to data string buffer
      if (str.length == fieldNames.length) {
        data += str.toList.mkString(",")
      }
    }

    data.toList
  }
}

class Employee(var Name: String, var ID: String, var Age: Int) {
  override def toString: String =
    s"($Name, $ID, $Age)"
}
