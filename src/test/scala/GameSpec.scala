import org.scalatest._

class GameSpec extends FlatSpec with Matchers {
  "A new game" should "not be finished, should not have a winner and should expect the move of White player" in {
    val g = Game.of(3, 3, 3)

    g.isFinished should be(false)
    g.winner should be(None)
    g.expects should be(White())
  }

  it should "be finished when the board is full" in {
    val b = Map(
      Position(0, 0) -> White(),
      Position(0, 1) -> Black(),
      Position(0, 2) -> White(),
      Position(1, 0) -> Black(),
      Position(1, 1) -> White(),
      Position(1, 2) -> Black(),
      Position(2, 0) -> White(),
      Position(2, 1) -> Black(),
    )
    val g = Game(3, 3, 3, Board(b, 3, 3), White(), isFinished = false, None)
    g.isFinished should be(false)

    val moveResult = g.move(Position(2, 2))

    moveResult.exists(_.isFinished) should be(true)
  }

  it should "be finished when there is a winner" in {
    val b = Map(
      Position(0, 0) -> White(),
      Position(0, 1) -> Black(),
      Position(1, 0) -> White(),
      Position(0, 2) -> Black(),
    )
    val g = Game(3, 3, 3, Board(b, 3, 3), White(), isFinished = false, None)
    g.isFinished should be(false)

    val moveResult = g.move(Position(2, 0))

    moveResult.exists(_.isFinished) should be(true)
    moveResult.exists(_.winner.contains(White())) should be(true)
  }

  it should "return an error if the position is already occupied" in {
    val b = Map(
      Position(0, 0) -> White(),
      Position(0, 1) -> Black(),
      Position(1, 0) -> White(),
      Position(0, 2) -> Black(),
    )
    val g = Game(5, 5, 3, Board(b, 3, 3), White(), isFinished = false, None)
    g.isFinished should be(false)

    val moveResult = g.move(Position(0, 0))

    moveResult.isLeft should be(true)
    moveResult.left.exists(_ == PositionOccupiedError(Position(0, 0))) should be(true)
  }
}