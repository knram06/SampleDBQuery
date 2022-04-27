object QueryHandler {
  var employeeDB = new EmployeeDB()
  employeeDB.initData()

  var allowedTableNames: Seq[String] = List[String] {"employees"}

  def printData(fields: Seq[String], filterObj: Option[Filter]): Unit = {
    val data = employeeDB.getData(fields.toList, filterObj)
    for (d <- data) {
       println(d)
    }
  }

  def main(args: Array[String]): Unit = {
    val parser = new SqlParser()
    val query = "select Name as N,Age as A from employees where Age >= 30"
    val selectObj = parser.parse(query)
    val projections = selectObj.projections

    var fieldNames = Array[String]()
    if (projections.length == 1 && projections.head.isInstanceOf[StarProj]) {
      fieldNames = employeeDB.allowedFieldNames
    }
    else {
      fieldNames = projections.map(p => p.asInstanceOf[FieldProj].field).toArray
    }

    println(selectObj)
    printData(fieldNames, selectObj.filter)
  }
}
