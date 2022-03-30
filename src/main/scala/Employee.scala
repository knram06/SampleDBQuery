class Employee(var Name: String, var ID: String, var Age: Int) {
  override def toString: String =
    s"($Name, $ID, $Age)"
}
