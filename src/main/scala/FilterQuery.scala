class FilterQuery(var field: String, var operator: String, var cmp: String) {

  val entries: List[String] = List[String](field, operator, cmp)
  if (!entries.forall(x => x == "") && entries.contains("")) {
    throw new Exception
  }

  def apply(inputField: String, inputVal: String): Boolean = {
    // if we are not at the field to match
    // return true
    if (field != inputField) {
      return true
    }

    operator match {
      case "lte" | "<=" => inputVal.toInt <= cmp.toInt
      case "lt"  | "<"  => inputVal.toInt < cmp.toInt
      case "gte" | ">=" => inputVal.toInt >= cmp.toInt
      case "gt"  | ">"  => inputVal.toInt > cmp.toInt
      case "eq"  | "=="  => inputVal == cmp
      case "startswith" => inputVal.startsWith(cmp)
      case "endswith"   => inputVal.endsWith(cmp)
      case _ =>
        println("Unknown operator! " + operator)
        throw new Exception
    }
  }
}
