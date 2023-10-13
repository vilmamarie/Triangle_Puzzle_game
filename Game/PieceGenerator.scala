package Game

import scala.util.Random

class PieceGenerator {
  var pieceString: Vector[String] = Vector[String]()

  //Pieces (and all their rotations) that have already been generated go here. This is checked to make sure no piece is added twice (there are no duplicates).
  private var checkerPieces: Vector[String] = Vector[String]()
  private var availableSides: String = "AaBbCcDd" //The chars which can be used to name sides.

  //The first piece created with random sides.
  private var firstPiece: String = s"${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}"


  def addTochecker(name: String): Unit = {
    checkerPieces = checkerPieces :+ name
    val newName = s"${name.last}" + name.take(2)
    checkerPieces = checkerPieces :+ newName
    checkerPieces = checkerPieces :+ (s"${newName.last}" + newName.take(2))
  }

  //Checks if a piece has already been generated.
  def check(name: String): Boolean = !checkerPieces.contains(name)

  //Changes a string to upper case if it is in lower case, and to lower case if it is in upper case. Useful for creating two sides that match.
  def matcher(p: String): String = if (p.toUpperCase == p) p.toLowerCase else p.toUpperCase

  //Generates a PuzzlePiece in the first row of the Puzzle. Uses matcher to create the sides corresponding to adjacent pieces depending on whether
  // the index (x) is even or not. Creates the rest of the sides randomly. Uses check to see if the piece has already been generated. If it has,
  // then it restarts the generation of this piece.
  def firstRowGenerate(x: Int): Unit = {
    var thePiece= ""
    if (x%2 == 0) {
        thePiece = matcher((pieceString(x-1).last).toString) + s"${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}"
      }else {
        thePiece = matcher((pieceString(x-1)(1)).toString) + s"${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}"
      }
    if (check(thePiece)) {
      addTochecker(thePiece)
      pieceString = pieceString :+ thePiece
    }else firstRowGenerate(x)
  }

  //Generates a PuzzlePiece in the second row of the Puzzle. Uses matcher to create the sides corresponding to adjacent pieces depending on whether
  // the index (x) is even, not even, or 5 (the first piece of the row). Creates the rest of the sides randomly. Uses check to see if the piece has
  // already been generated. If it has, then  it restarts the generation of this piece.
  def secondRowGenerate(x: Int): Unit = {
    var thePiece= ""
    if (x == 5) {
      thePiece = s"${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}"
      }else if (x%2 == 0) {
        thePiece = matcher((pieceString(x-1)(1)).toString) + matcher((pieceString(x-6).last).toString) + s"${availableSides(Random.nextInt(7))}"
      }else {
        thePiece = matcher((pieceString(x-1).last).toString) + s"${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}"
      }
    if (check(thePiece)) {
      addTochecker(thePiece)
      pieceString = pieceString :+ thePiece
    }else secondRowGenerate(x)
  }

  //Generates a PuzzlePiece in the third row of the Puzzle. Uses matcher to create the sides corresponding to adjacent pieces depending on whether
  // the index (x) is even, not even, or 12 (the first piece of the row). Creates the rest of the sides randomly. Uses check to see if the piece has
  // already been generated. If it has, then  it restarts the generation of this piece.
  def thirdRowGenerate(x: Int): Unit = {
    var thePiece= ""
    if (x == 12) {
      thePiece = s"${availableSides(Random.nextInt(7))}" + matcher((pieceString(x-7).last).toString) + s"${availableSides(Random.nextInt(7))}"
      }else if (x%2 == 0) {
        thePiece = matcher((pieceString(x-1)(1)).toString) + matcher((pieceString(x-7).last).toString) + s"${availableSides(Random.nextInt(7))}"
      }else {
        thePiece = matcher((pieceString(x-1).last).toString) + s"${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}"
      }
    if (check(thePiece)) {
      addTochecker(thePiece)
      pieceString = pieceString :+ thePiece
    }else thirdRowGenerate(x)
  }

  //Generates a PuzzlePiece in the fourth row of the Puzzle. Uses matcher to create the sides corresponding to adjacent pieces depending on whether
  // the index (x) is even, not even, or 19 (the first piece of the row). Creates the rest of the sides randomly. Uses check to see if the piece has
  // already been generated. If it has, then  it restarts the generation of this piece.
  def fourthRowGenerate(x: Int): Unit = {
    var thePiece= ""
    if (x == 19) {
      thePiece = s"${availableSides(Random.nextInt(7))}" + matcher((pieceString(x-6).last).toString) + s"${availableSides(Random.nextInt(7))}"
      }else if (x%2 == 0) {
        thePiece = matcher((pieceString(x-1).last).toString) + s"${availableSides(Random.nextInt(7))}${availableSides(Random.nextInt(7))}"
      }else {
        thePiece = matcher((pieceString(x-1)(1)).toString) + matcher((pieceString(x-6).last).toString) + s"${availableSides(Random.nextInt(7))}"
      }
    if (check(thePiece)) {
      addTochecker(thePiece)
      pieceString = pieceString :+ thePiece
    }else fourthRowGenerate(x)
  }

  //Does the actual generating using the above methods. Creates a vector of strings which correspond to sides of PuzzlePieces. This becomes
  // piecesInPile in the Puzzle class.
  def generate: Unit = {
    // Add first piece
    pieceString = pieceString :+ firstPiece
    addTochecker(firstPiece)

    // Create first row
    for (x <- 1 to 4) {
      firstRowGenerate(x)
    }

    // Create second row
    for (x <- 5 to 11) {
      secondRowGenerate(x)
    }

    // Create third row
    for (x <- 12 to 18) {
      thirdRowGenerate(x)
    }

    // Create fourth row
    for (x <- 19 to 23) {
      fourthRowGenerate(x)
    }
  }
}