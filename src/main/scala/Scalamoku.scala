import scala.annotation.tailrec
import scala.io.StdIn

object Scalamoku {
  def main(args: Array[String]): Unit = {
    loop(Game.of(rows = 5, columns = 5, requiredInRow = 3), None)
  }

  @tailrec
  def loop(game: Game, error: Option[GomokuError]): Unit = {
    printBoard(game.board)

    if (game.isFinished) {
      val r = game.winner.map(w => s"and the winner is $w.").getOrElse("and this is the draw!")
      println(s"The game is finished ${r}")
      return
    }

    if (error.isDefined) {
      Console.err.println("Error: " + error.get.message)
    }
    val command = StdIn.readLine(s"Expects the move of ${game.expects} (type 'quit' to exit):").trim
    if (command == "quit") {
      return
    }

    val parts = command.split(",")
    if (parts.length == 2) {
      val position = Position(parts(0).trim.toInt, parts(1).trim.toInt)
      game.move(position) match {
        case Right(g) => loop(g, None)
        case Left(e) => loop(game, Some(e))
      }
    } else {
      loop(game, error)
    }
  }

  def printBoard(board: Board): Unit = {
    for (row <- 0 until board.rows) {
      for (column <- 0 until board.columns) {
        board.at(Position(row, column)) match {
          case Some(White()) => print(" W ")
          case Some(Black()) => print(" B ")
          case None => print(" . ")
        }
      }
      println()
    }
  }
}

object Game {
  def of(rows: Integer, columns: Integer, requiredInRow: Integer): Game = Game(rows, columns, requiredInRow, Board(Map(), rows, columns), White(), isFinished = false, None)
}

case class Game(rows: Integer, columns: Integer, requiredInRow: Integer, board: Board, expects: Stone, isFinished: Boolean, winner: Option[Stone]) {
  def move(position: Position): Either[GomokuError, Game] =
    if (isFinished) {
      Left(GameFinishedError())
    } else {
      board.set(position, expects).map { updatedBoard =>
        val won = Position.directions.exists(hasInRow(position, expects, updatedBoard, _))
        val winner = if (won) Some(expects) else None
        val finished = updatedBoard.isFull || won

        Game(rows, columns, requiredInRow, updatedBoard, expects.opposite, finished, winner)
      }
    }

  @tailrec
  private def hasInRow(p: Position, s: Stone, b: Board, d: (Position) => Position, c: Integer = 1): Boolean = {
    val next = d(p)
    c == requiredInRow || (board.at(next).contains(s) && hasInRow(next, s, b, d, c + 1))
  }
}

case class Board(stones: Map[Position, Stone], rows: Integer, columns: Integer) {
  def at(position: Position): Option[Stone] =
    if (position.in(rows, columns)) {
      stones.get(position)
    } else {
      None
    }

  def isFull: Boolean = stones.size >= (rows * columns)

  def set(position: Position, stone: Stone): Either[GomokuError, Board] =
    if (isFull) {
      Left(FullBoardError())
    } else if (!position.in(rows, columns)) {
      Left(PositionOutOfRangeError(position, rows, columns))
    } else if (stones.contains(position)) {
      Left(PositionOccupiedError(position))
    } else {
      Right(Board(stones + (position -> stone), rows, columns))
    }
}

object Position {
  def directions: List[(Position) => Position] = List(
    _.up,
    _.upRight,
    _.right,
    _.rightDown,
    _.down,
    _.downLeft,
    _.left,
    _.upLeft
  )
}

case class Position(row: Integer, column: Integer) {
  def up: Position = Position(row - 1, column)

  def upRight: Position = Position(row - 1, column + 1)

  def right: Position = Position(row, column + 1)

  def rightDown: Position = Position(row - 1, column + 1)

  def down: Position = Position(row - 1, column)

  def downLeft: Position = Position(row - 1, column - 1)

  def left: Position = Position(row, column - 1)

  def upLeft: Position = Position(row + 1, column - 1)

  def in(rows: Integer, columns: Integer): Boolean = row >= 0 && row < rows && column >= 0 && column < columns
}

trait GomokuError {
  def message: String = ???
}

case class GameFinishedError() extends GomokuError {
  override def message: String = "the game is already finished"
}

case class FullBoardError() extends GomokuError {
  override def message: String = "the board is full"
}

case class PositionOccupiedError(position: Position) extends GomokuError {
  override def message: String = s"the position (${position.row}, ${position.column}) is already occupied"
}

case class PositionOutOfRangeError(position: Position, rows: Integer, columns: Integer) extends GomokuError {
  override def message: String = s"the position (${position.row}, ${position.column}) is out of range (rows = ${rows}, columns = ${columns})"
}

trait Stone {
  def opposite: Stone = ???
}

case class White() extends Stone {
  override def opposite: Stone = Black()

  override def toString: String = "White"
}

case class Black() extends Stone {
  override def opposite: Stone = White()

  override def toString: String = "Black"
}

