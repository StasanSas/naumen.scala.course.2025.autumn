
import scala.collection.mutable.HashMap

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  override def toString : String = getValueRefCeil(Queue.empty[(Int, Int, Table)], ix, iy, table)

  def getValueRefCeil(way : Queue[(Int, Int, Table)], ix: Int, iy: Int, table: Table): String = {
    val valueFromTable = table.getCell((ix, iy))
    (way.isEmpty, way.head, valueFromTable) match {
      case (_, _, None) => "outOfRange"
      case (true, _, Some(v)) => {
        val newWay = way.enqueue((ix, iy, table))
        getValueCeil(v, newWay)
      }
      case (false, Some((hx, hy, hTable)), Some(v)) => {
        if (hx == ix && hy == iy && hTable eq table){
          "cyclic"
        } else{
          val newWay = way.enqueue((ix, iy, table))
          getValueCeil(v, newWay)
        }
      }
    }
  }

  def getValueCeil(cell : Cell, way : Queue[(Int, Int, Table)]): String =  cell match {
    case ReferenceCell(ix, iy, table) => getValueRefCeil(way, ix, iy, table)
    case ceil => ceil.toString
  }
}


class Table(val width: Int, val height : Int) {
    private var data: HashMap[(Int, Int), Cell] = new HashMap()

   def getCell(ix: Int, iy: Int): Option[Cell] = {
     val isNotInBorders = ix < 0 || ix >= width || iy < 0 || iy >= height
     if (isNotInBorders) {
       None
     } else {
       data.get((ix, iy)).orElse(Some(EmptyCell))
     }
   }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    val isInBorders = ix >= 0 && ix < width && iy >= 0 && iy < height
    if (isInBorders) {
      data((ix, iy)) = cell
    }
  }
}