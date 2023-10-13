package Game

import scala.collection.mutable
import scala.util.Random

class Puzzle(val puzzleSlots: Map[(String, (Int, Int)), Int]) { //The pair of Int in the instance variables is a position, the string is either "up" or "down" (the direction).
  private var pilePieces: Vector[PuzzlePiece] = Vector[PuzzlePiece]()
  private var boardPieces: mutable.Buffer[Option[PuzzlePiece]] = mutable.Buffer(None, None, None, None, None, None, None, None, None, None, None, None, None,
    None, None, None, None, None, None, None, None, None, None, None)
  private var allOfThePieces: Array[PuzzlePiece] = pilePieces.toArray ++ boardPieces.filter(_.isDefined).map(_.get).toArray

  def allPieces: Array[PuzzlePiece] = this.allOfThePieces

  def piecesOnBoard: mutable.Buffer[Option[PuzzlePiece]] = boardPieces

  def piecesInPile: Vector[PuzzlePiece] = pilePieces

  //Generates the PuzzlePieces of this Puzzle.
  def generatePieces: Unit = {
    this.boardPieces = this.boardPieces.map( x => None)
    this.pilePieces = Vector[PuzzlePiece]()
    val generator = new PieceGenerator
    generator.generate
    //Shuffle the PuzzlePieces and switch the initial "rotationDir" of the puzzlePieces in piecesInPile to the correct ones, i.e "up" or "down" depending on the slot:
    var aPile = Random.shuffle(generator.pieceString.map( x => new PuzzlePiece(x(0), x(1), x(2)))).toBuffer
    for (x <- 0 to 4) {
      if (x%2 != 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    for (x <- 5 to 11) {
      if (x%2 == 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    for (x <- 12 to 18) {
      if (x%2 == 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    for (x <- 19 to 23) {
      if (x%2 != 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    //Rotate the pieces randomly:
    this.pilePieces = aPile.toVector.map( x => x.rotateMultipleTimes(Random.nextInt(7)))
    allPiecesupdate
  }

  def allPiecesupdate: Unit = this.allOfThePieces = this.pilePieces.toArray ++ this.boardPieces.filter(_.isDefined).map(_.get).toArray

  //Moves a piece to the first place in the allPieces array:
  // This allows for the piece which is being moved in the GUI to stay on top of all the other pieces.
  def moveToFirstPlace(piece: PuzzlePiece): Unit = {
    this.allOfThePieces = piece +: this.allPieces.filterNot(_ == piece)
  }

  //Adds a piece to the board and removes it from the pile.
  def addPieceToBoard(piece: PuzzlePiece, place: Int): Unit = {
    this.boardPieces(place) = Option(piece)
    this.pilePieces = this.pilePieces.filter( _ != piece)
  }

  //Removes a piece from the board and adds it to the pile.
  def removePieceFromBoard(piece: PuzzlePiece): Unit = {
    if (this.boardPieces.contains(Option(piece))) {
      this.boardPieces(this.boardPieces.indexOf(Option(piece))) = None
      this.pilePieces = this.pilePieces :+ piece
    }
  }

  //Adds a piece to the pile without affecting the board.
  def addPieceToPile(piece: PuzzlePiece): Unit = {
    this.pilePieces = this.pilePieces :+ piece
  }

  //Checks for each piece in the puzzle whether it is on the board or in the pile and moves it to the corresponding collection inside the puzzle object:
  def boardCheck: Unit = {
    //Checks all the pieces in the pile collection and adds them to the board (Removing them from pile) if they fill a slot:
    this.pilePieces.foreach( x => if (this.fillsSlot(x).isDefined) this.addPieceToBoard(x, this.fillsSlot(x).get))
    //Checks all the pieces on the board and removes them from the board (adds them to the pile) if they do not fill a slot:
    this.boardPieces.filter(_.isDefined).map(_.get).foreach( x => if (this.fillsSlot(x).isEmpty) this.removePieceFromBoard(x))
  }

  //Checks if a PuzzlePiece fills a puzzleSlots slot in the Puzzle.
  def fillsSlot(piece: PuzzlePiece): Option[Int] = {
    this.puzzleSlots.get((piece.rotationDir, piece.pos))
  }

  //Returns the boardPieces as a vector of strings (the sides of PuzzlePieces) wrapped in an option. Returns None if the puzzle board is not full.
  def stringOfPieces: Option[Vector[String]] = {
    if (this.boardPieces.size == this.boardPieces.count(_.nonEmpty)) {
      Option(this.boardPieces.map(_.get).map(_.sides).toVector)
    }else None
  }

  //Checks if the puzzle is solved using the SolvedChecker class.
  def isSolved: Boolean = {
    if (this.stringOfPieces.isDefined) {
      val checker = new SolvedChecker(this.stringOfPieces.get)
      checker.check
    }else false
  }
}