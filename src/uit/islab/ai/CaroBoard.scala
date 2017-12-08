package uit.islab.ai

class CaroBoard(board: Array[Array[Player]]) {

  def this(stringBoard: Array[String]) = this(stringBoard.map(row => CaroBoard.getPlayersFromString(row)))
  def this(rows: Int, cols: Int) = this(Array.fill(rows, cols)(Blank): Array[Array[Player]])
  def this(size: Int) = this(size, size)

  val rowCount = board.length
  val columnCount = if (board.isEmpty) 0 else board(0).length
  val columnNameMapping = (0 until 5 * columnCount).map(n => (CaroBoard.numToAlpha(n), n)).toMap

  val numInARowNeeded: Int = {
    // so nuoc di thang hang can de chien thang
    // numbers chosen rather arbitrarily by me. I looked at this: http://en.wikipedia.org/wiki/M,n,k-game
    // and tried to pick numbers that more or less made sense
    if (rowCount <= 3 || columnCount <= 3) {
      // tic tac toe or bizarre tiny variants
      scala.math.min(rowCount, columnCount)
    } else if (rowCount <= 5) {
      // connect 4, sort of
      4
    } else if (rowCount <= 14) {
      // gomoku
      5
    } else {
      // connect6. Seems like a good place to leave it
      6
    }
  }

  // get board as collection of rows
  def rows: Seq[Array[Player]] = {
    for (r <- 0 until rowCount)
      yield board(r)
  }

  // get board as collection of columns
  def columns: Seq[Array[Player]] = {
    for (c <- 0 until columnCount) yield (
      for (r <- (0 until rowCount))
        yield board(r)(c)).toArray
  }

  //get board as collection of diagonals from left to right
  def diagonalsLTR: Seq[Array[Player]] = {
    for (offset <- (1 - columnCount) until columnCount) yield (
      for (row <- 0 until rowCount if offset + row < columnCount && offset + row > -1)
        yield (board(row)(row + offset))).toArray
  }

  //get board as collection of diagonals from right to left
  def diagonalsRTL: Seq[Array[Player]] = {
    for (offset <- 0 until rowCount + rowCount - 1) yield (
      for (col <- 0 until columnCount if offset - col < rowCount && offset - col > -1)
        yield (board(offset - col)(col))).toArray
  }

  // find the winner
  def determineWinner: GameResult.Value = {
    val winnerText = "Player %s won!"
    val checkForWinner = { array: Array[Player] =>
      CaroBoard.nInARow(numInARowNeeded, array) match {
        case Some(player) => return player match { // non-local return!
          case X => GameResult.X
          case O => GameResult.O
          case other => throw new Exception("Error, '" + other + "' is not a player.")
        }
        case None => // do nothing
      }
    }

    rows foreach checkForWinner
    columns foreach checkForWinner
    diagonalsLTR foreach checkForWinner
    diagonalsRTL foreach checkForWinner

    if (board.map(row => row.contains(Blank)).contains(true)) {
      return GameResult.NoResult
    }

    return GameResult.Tie
  }

  override def toString: String = {
    var boardRepresentation = ""

    def p = { str: String => boardRepresentation = boardRepresentation.concat(str + "\n") }

    val topLine = (1 until columnCount).foldLeft("   ┌")((acc, c) => acc.concat("───┬")).concat("───┐")
    val middleLine = (0 until columnCount).foldLeft("   │")((acc, c) => acc.concat("───│"))
    val bottomLine = (1 until columnCount).foldLeft("   └")((acc, c) => acc.concat("───┴")).concat("───┘")

    p("")
    p((0 until columnCount).foldLeft("     ")((acc, n) => acc.concat("%-4s".format(CaroBoard.numToAlpha(n)))))
    p(topLine)
    for (r <- 0 until rowCount) {
      var rowString = "%-3d".format(r).concat("│")
      for (c <- 0 until columnCount) {
        rowString = rowString.concat(" %s │".format(board(r)(c)))
      }
      p(rowString)
      if (r < rowCount - 1) {
        p(middleLine)
      }
    }
    p(bottomLine)
    p("")

    return boardRepresentation
  }

  // neu toa do chua vuot ra ngoai ban co va vi tri do dang BLANK
  def validMove(row: Int, col: Int): Boolean = {
    return row < rowCount && row >= 0 && col < columnCount && col >= 0 && board(row)(col) == Blank
  }

  // cap nhat lai nuoc di nguoi choi Player danh
  def update(row: Int, col: Int, player: Player) = {
    board(row)(col) = player
  }

  // get index of column from column's letter name
  def columnNumber(columnName: String): Int = {
    return columnNameMapping(columnName)
  }
}

object CaroBoard {

  // transform indexing number to letter
  def numToAlpha(number: Int): String = {
    var dividend = number + 1 // internally, treat 1 as A - just makes it easier
    var letters = ""
    var modulo = 0

    while (dividend > 0) {
      modulo = (dividend - 1) % 26
      letters = (65 + modulo).toChar + letters
      dividend = (dividend - modulo) / 26
    }

    return letters
  }

  def getPlayersFromString(row: String): Array[Player] = {
    row.map(char => { if (char == 'X') X: Player else if (char == 'O') O: Player else Blank: Player }).toArray
  }

  def threeInARow(list: List[Player]): Option[Player] = list match {
    case Nil => None
    case x :: y :: z :: tail if x == y && y == z && z != Blank => Some(z)
    case _ :: tail => threeInARow(tail)
  }

  def nInARow(n: Int, array: Array[Player]): Option[Player] = {
    for (i <- 0 until array.length - (n - 1)) {
      var allTrue = true;
      for (j <- i + 1 until i + n) {
        allTrue &= array(j - 1) == array(j)
      }
      if (allTrue && array(i) != Blank) {
        if (i > 0 && i + n < array.length && array(i - 1) != array(i) && array(i + n) != array(i)) // dieu kien chan hai dau thi khong thang
          return None
        else
          return Some(array(i))
      }
    }

    return None
  }

}