import scala.collection.immutable.Queue

trait Cell {
  def toString(): String
}

case object EmptyCell extends Cell{
  override def toString : String = "empty"
}

case class StringCell(value : String) extends Cell{
  override def toString : String = value
}

case class NumberCell(value : Int) extends Cell{
  override def toString : String = value.toString
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  override def toString() : String = getValueRefCeil(Queue.empty[(Int, Int, Table)], ix, iy, table)

  def getValueRefCeil(way : Queue[(Int, Int, Table)], ix: Int, iy: Int, table: Table): String = {
    val valueFromTable = table.getCell(ix, iy)
    (way.isEmpty, way.head, valueFromTable) match {
      case (_, _, None) => "outOfRange"
      case (true, _, Some(v)) => {
        val newWay = way.enqueue((ix, iy, table))
        getValueCeil(v, newWay)
      }
      case (false, (hx, hy, hTable), Some(v)) => {
        if (hx == ix && hy == iy && (hTable eq table)){
          "cyclic"
        } else{
          val newWay = way.enqueue((ix, iy, table))
          getValueCeil(v, newWay)
        }
      }
    }
  }

  def getValueCeil(cell : Cell, way : Queue[(Int, Int, Table)]): String =  cell match {
    case ReferenceCell(x, y, table) => getValueRefCeil(way, x, y, table)
    case ceil => ceil.toString
  }

}

