package uit.islab.ai

object Caro {
  val Position = """([A-Za-z]+)\s*(\d+)""".r

  def main(args: Array[String]): Unit = {
    val size = readBoardSize
    var board = new CaroBoard(size)
    println("\n" + board.numInARowNeeded + " in a row to win (" + size + "x" + size + " board)")

    var player: Player = X
    while (board.determineWinner == GameResult.NoResult) {
      println(board)
      println("Player %s's turn.".format(player))

      val (row, col) = readNextMove(board)
      board.update(row, col, player)

      player = if (player == X) O else X
    }

    println(board)
    println(GameResult.displayGameResult(board.determineWinner))
    println
  }

  private def readBoardSize: Int = {
    var size = -1

    while (size < 1) {
      try {
        print("Enter board size: ")
        size = Console.readInt
      } catch {
        case e: Throwable => { size = -1 }
      }
      if (size < 1) {
        println("Invalid board size. Please enter a number greater than 0.")
      }
    }

    return size
  }

  private def readNextMove(board: CaroBoard): (Int, Int) = {
    var validMove = false
    var col = -1
    var row = -1
    while (!validMove) {
      var input = ""
      try {
        print("Enter square: (e.g. A0): ")
        input = Console.readLine
        val Position(columnName, rowNumber) = input
        row = rowNumber.toInt
        col = board.columnNumber(columnName.toUpperCase)
      } catch {
        case e: Throwable => { println("Error reading input: Could not understand \"" + input + "\"") }
      }

      validMove = board.validMove(row, col)
      if (!validMove) {
        println("Can't move there, try again!\n")
      }
    }

    return (row, col)
  }
}