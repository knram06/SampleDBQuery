trait Operation extends Node
case class SimpleOp(op: String) extends Operation {
  override def toString: String = op
}


