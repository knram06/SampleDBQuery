object QueryHandler {
  val employeeRecords: Seq[Employee] = List[Employee] (
    new Employee("Ram", "123", 34),
    new Employee("Joe", "124", 22),
    new Employee("Tom", "345", 27),
    new Employee("Eric", "456", 32),
    new Employee("James", "456", 35)
  )

  val employeeTable = new Table("employees")
  employeeTable.addList(employeeRecords)

  val tableManager = new TableMgr
  tableManager.addTable(employeeTable.getTableName, employeeTable)
  val allowedTableNames: Seq[String] = tableManager.getTableNames

  def main(args: Array[String]): Unit = {
    val parser = new SqlParser()
    val query = "select TestHeader as SomeName from (select Name as TestHeader,Age as TestAge from (select * from employees) as employeeData where TestAge >= 30)"
    val selectObj = parser.parse(query)

    println(selectObj)
    println(selectObj.getData)
  }
}
