package Tests

import Game.PuzzlePiece
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PuzzlePieceTest extends AnyFlatSpec with Matchers { //Tests the methods in PuzzlePiece.
  val piece = new PuzzlePiece('A', 'b', 'c')

  "aPuzzlePiece" should "Have the correct initial sides and rotationDir." in {

    assertResult("Abc") {piece.sides}
    assertResult("up") {piece.rotationDir}

  }

  it should "Have the correct sides and rotationDir after one rotation." in {

    piece.rotateRight
    assertResult("cAb") {piece.sides}
    assertResult("down") {piece.rotationDir}

  }

  it should "Have the correct sides and rotationDir after multiple rotations." in {

    piece.rotateMultipleTimes(4)
    assertResult("Abc") {piece.sides}
    assertResult("down") {piece.rotationDir}

    piece.rotateMultipleTimes(3)
    assertResult("cAb") {piece.sides}
    assertResult("up") {piece.rotationDir}

  }

  it should "Have the correct rotation and unchanged sides after it's rotation is changed." in {

    piece.changeRotation
    assertResult("cAb") {piece.sides}
    assertResult("down") {piece.rotationDir}

  }

}
