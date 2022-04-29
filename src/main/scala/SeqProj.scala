trait SqlProj extends Node {
  def toString: String
  def apply(input: Schema): Schema
}
case class StarProj() extends SqlProj {
  override def toString = "*"

  override def apply(input: Schema): Schema = {
    input
  }
}
case class FieldProj(field: String, alias: String = null) extends SqlProj {
  override def toString: String = {
    if (alias != null) {
      s"FieldProj($field, $alias)"
    }
    else {
      s"FieldProj($field)"
    }
  }

  override def apply(input: Schema): Schema = {
    if (alias != null) {
      // find the corresponding field in the headers
      val index = input.headers.indexOf(field)

      input.headers.update(index, alias)
    }
    input
  }
}

case class SeqProj(projections: Seq[SqlProj]) extends SqlProj {
  override def toString: String = {
    projections.mkString(",")
  }

  override def apply(input: Schema): Schema = {
    // TODO: Improve special handling for *
    if (projections.length == 1 && projections.head.isInstanceOf[StarProj]) {
      return input
    }

    // generate a list of indices in the headers
    // for input fields
    val fieldsList = projections.map(p => p.asInstanceOf[FieldProj])
    val indicesList = fieldsList.map(p => input.headers.indexOf(p.field)).toList
    if (indicesList.contains(-1)) {
      println("Unexpected field name not present in headers")
      throw new Exception
    }

    // grab the corresponding headers
    // overwrite with alias if present
    val ret = new Schema
    for (fl <- fieldsList) {
      var header = fl.field
      if (fl.alias != null) {
        header = fl.alias
      }

      ret.headers.addOne(header)
    }

    // grab the corresponding records
    for (rec <- input.records) {
      val elems = indicesList.map(i => rec(i))
      ret.addEntry(elems)
    }

    ret
  }
}
