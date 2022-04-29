class TableMgr {
  var tableMap: Map[String, Table] = Map[String, Table]()

  def addTable(name: String, table: Table): Unit = {
    tableMap += (name -> table)
  }

  def getTableNames: Seq[String] = {
    tableMap.keys.toList
  }

  def getAllowedTableFields(name: String): Seq[String] = {
    val tableInstance = tableMap(name)
    tableInstance.getAllowedFieldNames
  }

  def getTableData(name: String): List[List[String]] = {
    val tableInstance = tableMap(name)
    tableInstance.getData
  }

  def getTableSchema(name: String): Schema = {
    val tableInstance = tableMap(name)
    tableInstance.getSchema
  }
}
