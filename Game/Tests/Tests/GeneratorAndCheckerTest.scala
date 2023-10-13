package Tests

import Game.{PieceGenerator, PuzzlePiece, SolvedChecker}

import scala.util.Random
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GeneratorAndCheckerTest extends AnyFlatSpec with Matchers { // Tests the pieceGenerator and solvedChecker.
  val generator = new PieceGenerator
  var stringPieces = generator.pieceString.mkString
  var tester1: Set[String] = Set[String]()

  "GeneratorAndChecker" should "Produce a solved puzzle with no duplicates when generating." in {
    generator.generate
    stringPieces = generator.pieceString.mkString

    //All the pieces without any order. Useful to check if the pieces are the same before and after shuffling (next test).
    tester1 = generator.pieceString.toSet
    val checker = new SolvedChecker(generator.pieceString)

    //A Vector containing each rotation of the sides of each puzzlePiece. If the PuzzlePiece sides consist of the same letter i.e. "aaa", there is only one rotation.
    val allPossibleRotations = generator.pieceString.flatMap(x => if (x(0) == x(1) && x(1) == x(2)) Array(x) else Array(x, s"${x.last}" + x.take(2), s"${x(1)}" + s"${x.last}" + s"${x(0)}"))

    //Check whether the pieceString is a solved puzzle.
    assertResult(true) {checker.check}

    //Checks that the number of elements in allPossibleRotations is the same as the number of distinct elements in it. This checks that there are no duplicate pieces.
    assertResult(allPossibleRotations.size) {allPossibleRotations.distinct.size}

    //From printing this it can be visually checked whether the generator actually produced a valid solution.
    println("The string as a puzzle:")
    println("          ____________________"
      + s"\n         /\\   ${stringPieces(4)}    /\\    ${stringPieces(10)}   /\\"
      + s"\n        /  \\      /  \\      /  \\"
      + s"\n       /${stringPieces(0)}  ${stringPieces(1)}\\${stringPieces(3)}  ${stringPieces(5)}/${stringPieces(6)}  ${stringPieces(7)}\\${stringPieces(9)}  ${stringPieces(11)}/${stringPieces(12)}  ${stringPieces(13)}\\"
      + s"\n      /      \\  /      \\  /      \\"
      + s"\n     /____${stringPieces(2)}___\\/____${stringPieces(8)}___\\/___${stringPieces(14)}____\\"
      + s"\n    /\\    ${stringPieces(19)}   /\\    ${stringPieces(25)}   /\\   ${stringPieces(31)}    /\\"
      + s"\n   /  \\      /  \\      /  \\      /  \\"
      + s"\n  /${stringPieces(15)}  ${stringPieces(16)}\\${stringPieces(18)}  ${stringPieces(20)}/${stringPieces(21)}  ${stringPieces(22)}\\${stringPieces(24)}  ${stringPieces(26)}/${stringPieces(27)}  ${stringPieces(28)}\\${stringPieces(30)}  ${stringPieces(32)}/${stringPieces(33)}  ${stringPieces(34)}\\"
      + s"\n /      \\  /      \\  /      \\  /      \\"
      + s"\n/____${stringPieces(17)}___\\/___${stringPieces(23)}____\\/___${stringPieces(29)}____\\/___${stringPieces(35)}____\\"
      + s"\n\\    ${stringPieces(37)}   /\\   ${stringPieces(43)}    /\\   ${stringPieces(49)}    /\\   ${stringPieces(55)}    /"
      + s"\n \\      /  \\      /  \\      /  \\      /"
      + s"\n  \\${stringPieces(36)}  ${stringPieces(38)}/${stringPieces(39)}  ${stringPieces(40)}\\${stringPieces(42)}  ${stringPieces(44)}/${stringPieces(45)}  ${stringPieces(46)}\\${stringPieces(48)}  ${stringPieces(50)}/${stringPieces(51)}  ${stringPieces(52)}\\${stringPieces(54)}  ${stringPieces(56)}/"
      + s"\n   \\  /      \\  /      \\  /      \\  /"
      + s"\n    \\/____${stringPieces(41)}___\\/___${stringPieces(47)}____\\/____${stringPieces(53)}___\\/"
      + s"\n     \\    ${stringPieces(58)}   /\\   ${stringPieces(64)}    /\\    ${stringPieces(70)}   /"
      + s"\n      \\      /  \\      /  \\      /"
      + s"\n       \\${stringPieces(57)}  ${stringPieces(59)}/${stringPieces(60)}  ${stringPieces(61)}\\${stringPieces(63)}  ${stringPieces(65)}/${stringPieces(66)}  ${stringPieces(67)}\\${stringPieces(69)}  ${stringPieces(71)}/"
      + s"\n        \\  /      \\  /      \\  /"
      + s"\n         \\/___${stringPieces(62)}____\\/___${stringPieces(68)}____\\/")
  }

  //It is possible, but very highly unlikely that once the shuffling is done the pile pieces would still be in an order which produces a solution.
  it should "Produce a non-solved puzzle in the pile where rotations are correct and the pieces stay the same when the pieces are shuffled." in {
    //The below methods are how the Puzzle class shuffles and rotates the pieces that get generated.
    var aPile = Random.shuffle(generator.pieceString.map(x => new PuzzlePiece(x(0), x(1), x(2)))).toBuffer
    for (x <- 0 to 4) {
      if (x % 2 != 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    for (x <- 5 to 11) {
      if (x % 2 == 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    for (x <- 12 to 18) {
      if (x % 2 == 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    for (x <- 19 to 23) {
      if (x % 2 != 0) {
        aPile(x) = aPile(x).changeRotation
      }
    }
    //The rotations of the pieces saved before they get rotated randomly (To make sure each piece has the right rotation initially).
    val rotations = aPile.toVector.map(_.rotationDir).mkString(", ")
    val pieceVector = aPile.toVector.map(x => x.rotateMultipleTimes(Random.nextInt(7)))

    //All the pieces without any order for the shuffled set of pieces.
    val tester2 = pieceVector.map(x => s"${x.side1}${x.side2}${x.side3}").toSet
    val stringVector = pieceVector.map(_.sides)
    stringPieces = stringVector.mkString
    val newChecker = new SolvedChecker(stringVector)

    //Check that the rotations are right.
    assertResult("up, down, up, down, up, up, down, up, down, up, down, up, down, up, down, up, down, up, down, down, up, down, up, down") {rotations}

    //Check that the puzzle is not solved.
    assertResult(false) {newChecker.check}

    //Check that the pieces instances are still the same pieces from before in the sets which have no order.
    assertResult(tester1) {tester2}

    //From printing this it can be visually checked whether the solution is false.
    println("The shuffled string as a puzzle:")
    println("          ____________________"
      + s"\n         /\\   ${stringPieces(4)}    /\\    ${stringPieces(10)}   /\\"
      + s"\n        /  \\      /  \\      /  \\"
      + s"\n       /${stringPieces(0)}  ${stringPieces(1)}\\${stringPieces(3)}  ${stringPieces(5)}/${stringPieces(6)}  ${stringPieces(7)}\\${stringPieces(9)}  ${stringPieces(11)}/${stringPieces(12)}  ${stringPieces(13)}\\"
      + s"\n      /      \\  /      \\  /      \\"
      + s"\n     /____${stringPieces(2)}___\\/____${stringPieces(8)}___\\/___${stringPieces(14)}____\\"
      + s"\n    /\\    ${stringPieces(19)}   /\\    ${stringPieces(25)}   /\\   ${stringPieces(31)}    /\\"
      + s"\n   /  \\      /  \\      /  \\      /  \\"
      + s"\n  /${stringPieces(15)}  ${stringPieces(16)}\\${stringPieces(18)}  ${stringPieces(20)}/${stringPieces(21)}  ${stringPieces(22)}\\${stringPieces(24)}  ${stringPieces(26)}/${stringPieces(27)}  ${stringPieces(28)}\\${stringPieces(30)}  ${stringPieces(32)}/${stringPieces(33)}  ${stringPieces(34)}\\"
      + s"\n /      \\  /      \\  /      \\  /      \\"
      + s"\n/____${stringPieces(17)}___\\/___${stringPieces(23)}____\\/___${stringPieces(29)}____\\/___${stringPieces(35)}____\\"
      + s"\n\\    ${stringPieces(37)}   /\\   ${stringPieces(43)}    /\\   ${stringPieces(49)}    /\\   ${stringPieces(55)}    /"
      + s"\n \\      /  \\      /  \\      /  \\      /"
      + s"\n  \\${stringPieces(36)}  ${stringPieces(38)}/${stringPieces(39)}  ${stringPieces(40)}\\${stringPieces(42)}  ${stringPieces(44)}/${stringPieces(45)}  ${stringPieces(46)}\\${stringPieces(48)}  ${stringPieces(50)}/${stringPieces(51)}  ${stringPieces(52)}\\${stringPieces(54)}  ${stringPieces(56)}/"
      + s"\n   \\  /      \\  /      \\  /      \\  /"
      + s"\n    \\/____${stringPieces(41)}___\\/___${stringPieces(47)}____\\/____${stringPieces(53)}___\\/"
      + s"\n     \\    ${stringPieces(58)}   /\\   ${stringPieces(64)}    /\\    ${stringPieces(70)}   /"
      + s"\n      \\      /  \\      /  \\      /"
      + s"\n       \\${stringPieces(57)}  ${stringPieces(59)}/${stringPieces(60)}  ${stringPieces(61)}\\${stringPieces(63)}  ${stringPieces(65)}/${stringPieces(66)}  ${stringPieces(67)}\\${stringPieces(69)}  ${stringPieces(71)}/"
      + s"\n        \\  /      \\  /      \\  /"
      + s"\n         \\/___${stringPieces(62)}____\\/___${stringPieces(68)}____\\/")

  }


}
