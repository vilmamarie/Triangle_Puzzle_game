package Game.GUI

import Game.{Puzzle, PuzzlePiece}

import java.io._

object IO {

  //Loads a puzzle from a file. The variable file is the name of the file.
  def loadPuzzle(file: String) = {

    try {
      val reader = new FileReader(file)

      val lineReader = new BufferedReader(reader)

      try {
        //The first line needs to be the header:
        val header = lineReader.readLine().trim.toLowerCase

        //The map of slots for the returned puzzle object which will be filled later in this method:
        var slotMap = Map[(String, (Int, Int)), Int]()

        if (header != "puzzle") {
          throw new CorruptedPuzzleFileException("Unknown file type.")
        }

        var currentLine = lineReader.readLine().trim.toLowerCase

        //Goes through blank lines without doing anything.
        while (currentLine.isBlank) {
          currentLine = lineReader.readLine().trim.toLowerCase
        }

        if (currentLine != "puzzleslots:") {
          throw new CorruptedPuzzleFileException("Unknown file type.")
        }

        currentLine = lineReader.readLine().trim.toLowerCase

        //Extracts the slotMap from the file one slot at a time.
        while (!currentLine.isBlank && currentLine != "boardpieces:") {
          var direction = currentLine.takeWhile(_ != ',').trim.toLowerCase //The rotation "up" or "down"
          var firstCoordinate = ""
          var secondCoordinate = ""
          var index = "" //The number of the slot.

          direction match { //If the direction is not up or down there is something wrong with the file.
            case "up" => //Extract the coordinates and the slot number from the line, making sure they only contain numbers:
              firstCoordinate = currentLine.dropWhile(_ != '(').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              secondCoordinate = currentLine.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              index = currentLine.dropWhile(_ != ')').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim

            case "down" => //Extract the coordinates and the slot number from the line, making sure they only contain numbers:
              firstCoordinate = currentLine.dropWhile(_ != '(').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              secondCoordinate = currentLine.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              index = currentLine.dropWhile(_ != ')').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim

            case _ => //This means something is wrong with the file.
              throw new CorruptedPuzzleFileException("Unknown file type.")
          }
          if (!firstCoordinate.isBlank && !secondCoordinate.isBlank && !index.isBlank) { //If the Boolean evaluates to false it means some numeric values were missing.
            slotMap += (direction, (firstCoordinate.toInt, secondCoordinate.toInt)) -> index.toInt
            currentLine = lineReader.readLine().trim.toLowerCase
          }else throw new CorruptedPuzzleFileException("Missing values in file.")
        }

        //Create the puzzle object with the slotMap:
        val puzzle = new Puzzle(slotMap)

        while (currentLine.isBlank) {
          currentLine = lineReader.readLine().trim.toLowerCase
        }

        if (currentLine != "boardpieces:") {
          throw new CorruptedPuzzleFileException("Unknown file type.")
        }

        currentLine = lineReader.readLine().trim

        //Extracts the pieces that are on the board from the file:
        while (!currentLine.isBlank && currentLine.toLowerCase != "pilepieces:") {
          var direction = currentLine.takeWhile(_ != ',').trim.toLowerCase //The rotation "up" or "down"
          var pieceSides = "" //The sides string i.e. "aBc"
          var firstCoordinate = ""
          var secondCoordinate = ""
          var index = "" //The index at which the piece is on the board

          direction match {//If the direction is not up or down there is something wrong with the file.
            case "up" => //Extract the sides, position and index of each piece, making sure the sides are of correct length and have the right contents,
              // and the coordinates and index consist of numbers:
              pieceSides = currentLine.dropWhile(_ != ',').drop(1).trim.takeWhile(_ != ',').trim
              if (pieceSides.length != 3 || !pieceSides.forall(x => "abcdABCD".contains(x))) {
                throw new CorruptedPuzzleFileException("Unknown PuzzlePiece sides in file.")
              }
              index = currentLine.dropWhile(_ != ',').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              firstCoordinate = currentLine.dropWhile(_ != '(').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              secondCoordinate = currentLine.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              val pieceToAdd = new PuzzlePiece(pieceSides(0), pieceSides(1), pieceSides(2))
              if (!firstCoordinate.isBlank && !secondCoordinate.isBlank && !index.isBlank) { //If the Boolean evaluates to false it means some numeric values were missing.
                puzzle.addPieceToBoard(pieceToAdd, index.toInt) //Add the piece to the board.
                pieceToAdd.changePos(firstCoordinate.toInt, secondCoordinate.toInt) //Change the coordinates of the piece from the default ones.
              }else throw new CorruptedPuzzleFileException("Missing values in file.")

            case "down" => //Extract the sides, position and index of each piece, making sure the sides are of correct length and have the right contents,
              // and the coordinates and index consist of numbers:
              pieceSides = currentLine.dropWhile(_ != ',').drop(1).trim.takeWhile(_ != ',').trim
              if (pieceSides.length != 3 || !pieceSides.forall(x => "abcdABCD".contains(x))) {
                throw new CorruptedPuzzleFileException("Unknown PuzzlePiece sides in file.")
              }
              index = currentLine.dropWhile(_ != ',').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              firstCoordinate = currentLine.dropWhile(_ != '(').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              secondCoordinate = currentLine.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              val pieceToAdd = new PuzzlePiece(pieceSides(0), pieceSides(1), pieceSides(2))
              if (!firstCoordinate.isBlank && !secondCoordinate.isBlank && !index.isBlank) { //If the Boolean evaluates to false it means some numeric values were missing.
                puzzle.addPieceToBoard(pieceToAdd.changeRotation, index.toInt) //Add the piece to the board and change the default rotation.
                pieceToAdd.changePos(firstCoordinate.toInt, secondCoordinate.toInt) //Change the coordinates of the piece from the default ones.
              }else throw new CorruptedPuzzleFileException("Missing values in file.")

            case _ => //This means something is wrong with the file.
              throw new CorruptedPuzzleFileException("Unknown file type.")
          }
          currentLine = lineReader.readLine().trim
        }

        while (currentLine.isBlank) {
          currentLine = lineReader.readLine().trim.toLowerCase
        }

        if (currentLine.toLowerCase != "pilepieces:") {
          throw new CorruptedPuzzleFileException("Unknown file type.")
        }

        currentLine = lineReader.readLine().trim

        //Extracts the pieces that are in the pile from the file:
        while (!currentLine.isBlank && currentLine.toLowerCase != "stop") {
          var direction = currentLine.takeWhile(_ != ',').trim.toLowerCase //The rotation "up" or "down"
          var pieceSides = "" //The sides string i.e. "aBc"
          var firstCoordinate = ""
          var secondCoordinate = ""

          direction match { //If the direction is not up or down there is something wrong with the file. Does almost the exact same thing as the previous
              // while loop except without the index variable.
            case "up" =>
              pieceSides = currentLine.dropWhile(_ != ',').drop(1).trim.takeWhile(_ != ',').trim
              if (pieceSides.length != 3 || !pieceSides.forall(x => "abcdABCD".contains(x))) {
                throw new CorruptedPuzzleFileException("Unknown PuzzlePiece sides in file.")
              }
              firstCoordinate = currentLine.dropWhile(_ != '(').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              secondCoordinate = currentLine.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              val pieceToAdd = new PuzzlePiece(pieceSides(0), pieceSides(1), pieceSides(2))
              if (!firstCoordinate.isBlank && !secondCoordinate.isBlank) {
                puzzle.addPieceToPile(pieceToAdd) //Add the piece to the pile.
                pieceToAdd.changePos(firstCoordinate.toInt, secondCoordinate.toInt) //Change the coordinates of the piece from the default ones.
              }else  throw new CorruptedPuzzleFileException("Missing values in file.")
            case "down" =>
              pieceSides = currentLine.dropWhile(_ != ',').drop(1).trim.takeWhile(_ != ',').trim
              if (pieceSides.length != 3 || !pieceSides.forall(x => "abcdABCD".contains(x))) {
                throw new CorruptedPuzzleFileException("Unknown PuzzlePiece sides in file.")
              }
              firstCoordinate = currentLine.dropWhile(_ != '(').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              secondCoordinate = currentLine.dropWhile(_ != '(').drop(1).dropWhile(_ != ',').drop(1).trim.takeWhile( x => "0123456789".contains(x) ).trim
              val pieceToAdd = new PuzzlePiece(pieceSides(0), pieceSides(1), pieceSides(2))
              if (!firstCoordinate.isBlank && !secondCoordinate.isBlank) {
                puzzle.addPieceToPile(pieceToAdd.changeRotation) //Add the piece to the pile and change its rotation.
                pieceToAdd.changePos(firstCoordinate.toInt, secondCoordinate.toInt) //Change the coordinates of the piece from the default ones.
              }else  throw new CorruptedPuzzleFileException("Missing values in file.")
            case _ =>
              throw new CorruptedPuzzleFileException("Unknown file type.")
          }
          currentLine = lineReader.readLine().trim
        }

        //Return the final puzzle and close the readers.
        puzzle
      } finally {
        reader.close()
        lineReader.close()
      }
    } catch {
      case notFound: FileNotFoundException => "File not found."

      case exception: IOException => "Issue with IO."

      case puzzleException: CorruptedPuzzleFileException => puzzleException.explanation //Returns the message from the exception.

      case empty: NullPointerException => "Empty file."

      case _: Throwable => "Unexpected Exception."
    }
  }

  //Writes a puzzle to a file. The name of the file is the variable file.
  def writePuzzle(file: String, puzzle: Puzzle) = {
    try {
      val writer = new FileWriter(file)
      val lineWriter = new BufferedWriter(writer)

      //Writes all the information in the correct order:
      try {
        lineWriter.write("PUZZLE")
        lineWriter.newLine()
        lineWriter.write("PuzzleSlots:")
        lineWriter.newLine()
        for (x <- puzzle.puzzleSlots.toVector) {
          val slot = x._1._1 + s", (${x._1._2._1}, ${x._1._2._2}), ${x._2}" //This corresponds to "direction, (coordinate 1, coordinate 2), index" for example "up, (1, 2), 3"
          lineWriter.write(slot)
          lineWriter.newLine()
        }
        lineWriter.write("BoardPieces:")
        lineWriter.newLine()
        for (x <- puzzle.piecesOnBoard) {
          if (x.isDefined) {
            //This corresponds to "direction, sides, index, (coordinate 1, coordinate 2)" for example "up, abc, 3, (1,2)".
            val piece = x.get.rotationDir + ", " + x.get.sides + s", ${puzzle.piecesOnBoard.indexOf(x)}, (${x.get.pos._1}, ${x.get.pos._2})"
            lineWriter.write(piece)
            lineWriter.newLine()
          }
        }
        lineWriter.write("PilePieces:")
        lineWriter.newLine()
        for (x <- puzzle.piecesInPile) {
          // This corresponds to "direction, sides, (coordinate 1, coordinate 2)" for example "up, abc, (1, 2)"
          val piece = x.rotationDir + ", " + x.sides + s", (${x.pos._1}, ${x.pos._2})"
          lineWriter.write(piece)
          lineWriter.newLine()
        }
        lineWriter.write("STOP")
      } finally {
        writer.flush()
        lineWriter.flush()
        writer.close()
        lineWriter.close()
      }
    } catch {
      case notfound: FileNotFoundException =>
        "File not found."
      case exception: IOException =>
        "Issue with IO."
      case _: Throwable =>
        "Unexpected Exception."
    }
  }
}
