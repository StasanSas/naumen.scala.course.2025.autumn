import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    println("=== Тестирование таблицы ===")

    // Создаем таблицу 3x3
    val table = new Table(3, 3)

    // Заполняем ячейки
    table.setCell(0, 0, StringCell("Hello"))
    table.setCell(0, 1, NumberCell(42))
    table.setCell(1, 0, StringCell("World"))
    table.setCell(2, 2, NumberCell(100))

    // Тестируем получение ячеек
    println("=== Чтение ячеек ===")
    for {
      x <- 0 until 3
      y <- 0 until 3
    } {
      val cell = table.getCell(x, y)
      println(s"Ячейка ($x, $y): $cell")
    }

    // Тестируем граничные случаи
    println("\n=== Граничные случаи ===")
    println(s"За пределами: ${table.getCell(5, 5)}")
    println(s"Отрицательные: ${table.getCell(-1, 0)}")

    // Тестируем EmptyCell
    println(s"\n=== Пустая ячейка ===")
    println(s"Пустая (1,1): ${table.getCell(1, 1)}")
  }
}