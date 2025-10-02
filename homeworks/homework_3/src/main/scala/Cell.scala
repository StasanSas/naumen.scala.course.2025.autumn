
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


