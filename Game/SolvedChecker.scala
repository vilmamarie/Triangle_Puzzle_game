package Game

class SolvedChecker(val pieceString: Vector[String]) {

  //Changes a string to upper case if it is in lower case, and to lower case if it is in upper case. Useful for checking if two sides match.
  def matcher(p: String): String = if (p.toUpperCase == p) p.toLowerCase else p.toUpperCase

  //Checks if a PuzzlePiece in the first row of the Puzzle is in the right place. If the index (x) is even, it checks if the first side matches the last side of the previous
  //piece, and if not, checks if the first side matches the second side of the previous piece.
  def firstRowCheck(x: Int): Boolean = {
    if (x%2 == 0) {
        matcher((this.pieceString(x-1).last).toString) == this.pieceString(x)(0).toString
      }else {
        matcher((this.pieceString(x-1)(1)).toString) == this.pieceString(x)(0).toString
      }
  }

  //Checks if a PuzzlePiece in the second row of the Puzzle is in the right place. If the index (x) is even, it checks if the first side matches the second side of the previous
  //piece and if the second side matches the last side of the piece at x-6. If x is not even, checks if the first side matches the last side of the previous piece.
  def secondRowCheck(x: Int): Boolean = {
    if (x%2 == 0) {
        (matcher((this.pieceString(x-1)(1)).toString) == this.pieceString(x)(0).toString) && (matcher((this.pieceString(x-6).last).toString) == this.pieceString(x)(1).toString)
      }else {
        matcher((this.pieceString(x-1).last).toString) == this.pieceString(x)(0).toString
      }
  }

  //Checks if a PuzzlePiece in the third row of the Puzzle is in the right place. If the index (x) is even, it checks if the first side matches the second side of the previous
  //piece and if the second side matches the last side of the piece at x-7. If x is not even, checks if the first side matches the last side of the previous piece. If x is 12,
  //it is the first piece of this row, and the method checks if the second side matches the last side of the piece at x-7.
  def thirdRowCheck(x: Int): Boolean = {
    if (x == 12) {
        matcher((this.pieceString(x-7).last).toString) == this.pieceString(x)(1).toString
      }else if (x%2 == 0) {
        (matcher((this.pieceString(x-1)(1)).toString) == this.pieceString(x)(0).toString) && (matcher((this.pieceString(x-7).last).toString) == this.pieceString(x)(1).toString)
      }else {
        matcher((this.pieceString(x-1).last).toString) == this.pieceString(x)(0).toString
      }
  }

  //Checks if a PuzzlePiece in the fourth row of the Puzzle is in the right place. If the index (x) is not even, it checks if the first side matches the second side of the previous
  //piece and if the second side matches the last side of the piece at x-7. If x is even, checks if the first side matches the last side of the previous piece. If x is 19,
  //it is the first piece of this row, and the method checks if the second side matches the last side of the piece at x-6.
  def fourthRowCheck(x: Int): Boolean = {
    if (x == 19) {
        matcher((this.pieceString(x-6).last).toString) == this.pieceString(x)(1).toString
      }else if (x%2 == 0) {
        matcher((this.pieceString(x-1).last).toString) == this.pieceString(x)(0).toString
      }else {
        (matcher((this.pieceString(x-1)(1)).toString) == this.pieceString(x)(0).toString) && (matcher((this.pieceString(x-6).last).toString) == this.pieceString(x)(1).toString)
      }
  }

  //Does the actual checking using the above methods.
  def check: Boolean = {
    var truthValue = true

    //Stops the checker once it has gone through all the rows.
    var stop = false

    while (truthValue && !stop) {
      // Check first row
      for (x <- 1 to 4) {
        truthValue = truthValue && firstRowCheck(x)
      }

      // Check second row
      for (x <- 6 to 11) {
        truthValue = truthValue && secondRowCheck(x)
      }

      // Check third row
      for (x <- 12 to 18) {
        truthValue = truthValue && thirdRowCheck(x)
      }

      // Check fourth row
      for (x <- 19 to 23) {
        truthValue = truthValue && fourthRowCheck(x)
      }
      stop = true
    }
    truthValue
  }
}