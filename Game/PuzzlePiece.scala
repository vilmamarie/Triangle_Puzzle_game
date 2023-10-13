package Game



class PuzzlePiece(val side1: Char, val side2: Char, val side3: Char) {
  private var currentPos: (Int, Int) = (100, 200)

  //The rotation of the PuzzlePiece, i.e. "up" if the triangle is "pointing" upwards,
  //or "down" if the triangle is "pointing" downwards.
  private var rotation: String = "up"

  //The order in which the sides are.
  private var sidesOrder: String = s"${side1}${side2}${side3}"

  def pos: (Int, Int) = currentPos

  def changePos(a: Int, b: Int): Unit = {currentPos = (a,b)}

  //Rotates the piece once, meaning it changes the the rotation and the order of sidesOrder if the rotation is up. It does this
  // because when changing the rotation from down to up, the sides need to stay the same otherwise they don't change right. When
  // rotating from down to up, the "next position" for the first side is the first side of an "up" rotated triangle. This is hard
  // to explain in text and can be seen more clearly in the GUI when rotating pieces and in a diagram of how the sides are numbered
  // in the project document.
  def rotateRight: Unit = {
    rotation match {
      case "up" =>
        rotation = "down"
        sidesOrder = s"${sidesOrder.last}" + sidesOrder.take(2)
      case "down" =>
        rotation = "up"
      case _ =>
    }
  }

  //Rotates the piece "times" times, meaning it changes the order of sidesOrder and the rotation.
  def rotateMultipleTimes(times: Int): PuzzlePiece = {
    for (x <- 0 until times) {
      this.rotateRight
    }
    this
  }

  //Changes the rotation of the PuzzlePiece without changing the order of sidesOrder.
  // It is useful when initializing pieces.
  def changeRotation: PuzzlePiece = {
    rotation match {
      case "up" => rotation = "down"
      case "down" => rotation = "up"
      case _ => //Throw an exception maybe
    }
    this
  }

  def rotationDir: String = rotation

  def sides: String = sidesOrder

}