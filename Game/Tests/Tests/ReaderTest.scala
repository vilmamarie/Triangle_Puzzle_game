package Tests

import Game.GUI.IO
import Game.Puzzle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReaderTest extends AnyFlatSpec with Matchers { //Tests the file reader and writer. The results of the writer can be seen in the FileToBeWrittenTo.txt file.
  val puzzle = new Puzzle(Map(("down", (745, 242)) -> 10, ("up", (625, 328)) -> 13, ("down", (625, 413)) -> 19, ("down", (595, 328)) -> 12, ("down", (715, 157)) -> 3,
    ("down", (625, 242)) -> 6, ("down", (685, 413)) -> 21, ("down", (655, 157)) -> 1, ("up", (685, 158)) -> 2, ("up", (655, 243)) -> 7, ("up", (595, 243)) -> 5,
    ("up", (745, 328)) -> 17, ("up", (685, 328)) -> 15, ("up", (775, 243)) -> 11, ("down", (655, 328)) -> 14, ("up", (745, 158)) -> 4, ("down", (745, 413)) -> 23,
    ("down", (715, 328)) -> 16, ("up", (715, 413)) -> 22, ("down", (775, 328)) -> 18, ("up", (655, 413)) -> 20, ("up", (715, 243)) -> 9,("up", (625, 158)) -> 0,
    ("down", (685, 242)) -> 8))



  "ReaderTest" should "Be able to correctly write and load the slot map, board pieces, and pile pieces of the puzzle." in {
    puzzle.generatePieces
    //First write the generated puzzle into the file ReaderTestFile.txt:
    IO.writePuzzle("./Game/ExampleFiles/ReaderTestFile.txt", puzzle)
    //Then load that file into a new puzzle:
    val newPuzzle = IO.loadPuzzle("./Game/ExampleFiles/ReaderTestFile.txt")

    //Make sure that the puzzleSlots are the same in both the original puzzle and the loaded one:
    assertResult(puzzle.puzzleSlots) {newPuzzle.asInstanceOf[Puzzle].puzzleSlots}

    //Make sure the structure of piecesOnBoard is the same in both the original puzzle and the loaded one:
    assertResult(puzzle.piecesOnBoard) {newPuzzle.asInstanceOf[Puzzle].piecesOnBoard}

    //Make sure the pieces in piecesOnBoard are the same in both the original puzzle and the loaded one:
    // (This can't be done by simply equating them, because they are different instances even if they have
    // all the same features. Hence the features, meaning .sides, .rotationDir, and .pos are the ones checked to be matching.)
    assertResult(puzzle.piecesOnBoard.filter(_.isDefined).map(_.get).map(x => (x.rotationDir, x.sides, x.pos))) {newPuzzle.asInstanceOf[Puzzle].piecesOnBoard.filter(_.isDefined).map(_.get).map(x => (x.rotationDir, x.sides, x.pos))}

    //Make sure the pieces in piecesInPile are the same in both the original puzzle and the loaded one:
    // (This can't be done by simply equating them, because they are different instances even if they have
    // all the same features. Hence the features, meaning .sides, .rotationDir, and .pos are the ones checked to be matching.)
    assertResult(puzzle.piecesInPile.map(x => (x.rotationDir, x.sides, x.pos))) {newPuzzle.asInstanceOf[Puzzle].piecesInPile.map(x => (x.rotationDir, x.sides, x.pos))}

  }

  it should "Return the message from the thrown exception when reading an invalid file." in {

    //This file has a missing header:
    assertResult("Unknown file type.") {IO.loadPuzzle("./Game/ExampleFiles/BrokenPuzzleFile1.txt")}

    //This file has a missing direction in one of the puzzleSlots:
    assertResult("Unknown file type.") {IO.loadPuzzle("./Game/ExampleFiles/BrokenPuzzleFile2.txt")}

    //This file has a missing coordinate in one of the puzzleSlots:
    assertResult("Missing values in file.") {IO.loadPuzzle("./Game/ExampleFiles/BrokenPuzzleFile3.txt")}

    //This file has an invalid letter (e) in one of the sides of a piece in pilePieces:
    assertResult("Unknown PuzzlePiece sides in file.") {IO.loadPuzzle("./Game/ExampleFiles/BrokenPuzzleFile4.txt")}

    assertResult("Empty file.") {IO.loadPuzzle("./Game/ExampleFiles/BrokenPuzzleFile5.txt")}
  }

}
