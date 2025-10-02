import scala.collection.immutable.Queue

trait Cell {
  def toString(): String
}

case class EmptyCell() extends Cell{
  override def toString : String = "empty"
}

case class StringCell(value : String) extends Cell{
  override def toString : String = value
}

case class NumberCell(value : Int) extends Cell{
  override def toString : String = value.toString
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  override def toString() : String = getValueRefCell(Queue.empty[(Int, Int, Table)], ix, iy, table)

  def getValueRefCell(way : Queue[(Int, Int, Table)], ix: Int, iy: Int, table: Table): String = {
    val valueFromTable = table.getCell(ix, iy)
    (way.isEmpty, valueFromTable) match {
      case (_, None) => "outOfRange"
      case (true, Some(v)) => {
        val newWay = way.enqueue((ix, iy, table))
        getValueCell(v, newWay)
      }
      case (false, Some(v)) => {
        val (headX, headY, headTable) = way.head
        if (headX == ix && headY == iy && (headTable eq table)) {
          "cyclic"
        } else{
          val newWay = way.enqueue((ix, iy, table))
          getValueCell(v, newWay)
        }
      }
    }
  }

  def getValueCell(cell : Cell, way : Queue[(Int, Int, Table)]): String =  cell match {
    case ReferenceCell(x, y, table) => getValueRefCell(way, x, y, table)
    case cell => cell.toString
  }

}

