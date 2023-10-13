package Tests

import Game.Puzzle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PuzzleTest extends AnyFlatSpec with Matchers {
  val puzzle = new Puzzle(Map(("down", (745, 242)) -> 10, ("up", (625, 328)) -> 13, ("down", (625, 413)) -> 19, ("down", (595, 328)) -> 12, ("down", (715, 157)) -> 3,
    ("down", (625, 242)) -> 6, ("down", (685, 413)) -> 21, ("down", (655, 157)) -> 1, ("up", (685, 158)) -> 2, ("up", (655, 243)) -> 7, ("up", (595, 243)) -> 5,
    ("up", (745, 328)) -> 17, ("up", (685, 328)) -> 15, ("up", (775, 243)) -> 11, ("down", (655, 328)) -> 14, ("up", (745, 158)) -> 4, ("down", (745, 413)) -> 23,
    ("down", (715, 328)) -> 16, ("up", (715, 413)) -> 22, ("down", (775, 328)) -> 18, ("up", (655, 413)) -> 20, ("up", (715, 243)) -> 9,("up", (625, 158)) -> 0,
    ("down", (685, 242)) -> 8))

  "newPuzzle" should "Generate the right amount of pieces which have no duplicates into piecesInPile." in {
    puzzle.generatePieces

    //A Vector containing each rotation of the sides of each puzzlePiece. If the PuzzlePiece sides consist of the same letter i.e. "aaa", there is only one rotation.
    val allRotations = puzzle.piecesInPile.map(_.sides).flatMap(x => if (x(0) == x(1) && x(1) == x(2)) Array(x) else Array(x, s"${x.last}" + x.take(2), s"${x(1)}" + s"${x.last}" + s"${x(0)}"))

    //Make sure there are no duplicate pieces:
    assertResult(allRotations.size) {allRotations.distinct.size}

    //Make sure all 24 pieces were generated and are in the pile:
    assertResult(24) {puzzle.piecesInPile.size}

  }

  it should "Add pieces to the board with the addPieceToBoard method, and remove them from the pile at the same time. " +
    "Remove pieces from the board with the removePieceFromBoard method, and add them to the pile at the same time. The fillsSlot method should return the correct answer." in {
    val pieceToBeMoved = puzzle.piecesInPile(0)
    puzzle.addPieceToBoard(pieceToBeMoved, 3)

    //Make sure the piece at index 3 on the board is pieceToBeMoved:
    assertResult(puzzle.piecesOnBoard(3).get) {pieceToBeMoved}
    //Make sure the piece is no longer in the pile:
    assertResult(false) {puzzle.piecesInPile.contains(pieceToBeMoved)}

    puzzle.removePieceFromBoard(pieceToBeMoved)

    //Make sure the pieceToBeMoved is no longer on the board at index 3:
    assertResult(None) {puzzle.piecesOnBoard(3)}
    //Make sure the pieceToBeMoved was moved to the pile:
    assertResult(true) {puzzle.piecesInPile.contains(pieceToBeMoved)}
    //Make sure the pieceToBeMoved doesn't fill a slot:
    assertResult(None) {puzzle.fillsSlot(pieceToBeMoved)}
  }

  //The puzzle can technically be a solved puzzle at this point but that is very highly unlikely.
  // Hence there is a tiny chance this test fails even though everything works properly.
  it should "Not be a solved puzzle if pieces are placed on the board in the same order as the pile." in {
    //Add all the pieces to the board:
    var index = 0
    for (x <- puzzle.piecesInPile) {
      puzzle.addPieceToBoard(x, index)
      index = index + 1
    }

    //Check that the puzzle isn't solved:
    assertResult(false) {puzzle.isSolved}

    //Remove all the pieces from the board:
    puzzle.piecesOnBoard.map(_.get).foreach( x => puzzle.removePieceFromBoard(x))

    //Make sure there are no pieces on the board:
    assertResult(true) {!puzzle.piecesOnBoard.exists(_.isDefined)}
  }

  it should "Add pieces to the pile without affecting piecesOnBoard when addPieceToPile is used." in {
    val pieceToBeMoved = puzzle.piecesInPile(0)
    puzzle.addPieceToBoard(pieceToBeMoved, 3)
    puzzle.addPieceToPile(pieceToBeMoved)

    //Check that the pieceToBeMoved is in the pile:
    assertResult(true) {puzzle.piecesInPile.contains(pieceToBeMoved)}
    //Check that the pieceToBeMoved is still on the board:
    assertResult(pieceToBeMoved) {puzzle.piecesOnBoard(3).get}


  }
}
