package Game.GUI

import Game.{Puzzle, PuzzlePiece}

import java.awt.{Color, Font, Point, Polygon}
import javax.swing.{ImageIcon, JDialog, JOptionPane}
import scala.swing.GridBagPanel.Anchor.{NorthWest, SouthEast}
import scala.swing.GridBagPanel.Fill
import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked, MouseDragged, MousePressed, MouseReleased}
import scala.util.Random

object PuzzleGUI extends SimpleSwingApplication {

  //Map for building the slots of the puzzle.
  var puzzleMap: Map[(String, (Int, Int)), Int] = Map()
  //Adds all the key value pairs with the rotation and the coordinates mapping to the index of the slot.
  // Every other slot is orientated differently, which is why even and odd numbers are separated in the for loops.
  for (x <- 0 to 4) {
      if (x%2 == 0) {
        puzzleMap += ("up", (625 + (x/2)*60, 158)) -> x
      }else {
        puzzleMap += ("down", (655 + (x/2)*60, 157)) -> x
      }
    }

    for (x <- 5 to 11) {
      if (x%2 == 0) {
        puzzleMap += ("down", (625 + ((x-5)/2)*60, 242)) -> x
      }else {
        puzzleMap += ("up", (595 + ((x-5)/2)*60, 243)) -> x
      }
    }

    for (x <- 12 to 18) {
      if (x%2 == 0) {
        puzzleMap += ("down", (595 + ((x-12)/2)*60, 328)) -> x
      }else {
        puzzleMap += ("up", (625 + ((x-12)/2)*60, 328)) -> x
      }
    }

    for (x <- 19 to 23) {
      if (x%2 == 0) {
        puzzleMap += ("up", (655 + ((x-19)/2)*60, 413)) -> x
      }else {
        puzzleMap += ("down", (625 + ((x-19)/2)*60, 413)) -> x
      }
    }

  //The puzzle object:
  var puzzle = new Puzzle(puzzleMap)

  //Map from PuzzlePiece sides (Chars) to their respective colors. For example a piece "BbA" would have colors bright blue, light blue,
  // and bright red.
  val colorMap = Map[Char, Color]('A' -> Color.BLUE, 'a' -> new Color(95, 171, 226), 'B' -> new Color(39, 189, 39), 'b' -> new Color(134, 220, 134),
    'C' -> new Color(137, 27, 129), 'c' -> new Color(206, 114, 200), 'D' -> Color.RED, 'd' -> new Color(215, 94, 94))

  //Main frame:
  def top = new MainFrame {
    title = "Triangle puzzle"
    background = new Color(238, 234, 191)
    contents = new PuzzlePanel {
      layout += startOverButton -> new Constraints(2, 2, 4, 2, 1, 1, SouthEast.id, Fill.None.id, new Insets(8, 5, 30, 73), 0, 0)
      layout += saveButton -> new Constraints(4, 2, 4, 2, 1, 1, SouthEast.id, Fill.None.id, new Insets(8, 5, 30, 233), 0, 0)
      layout += startButton -> new Constraints(1, 1, 4, 2, 1, 1, NorthWest.id, Fill.None.id, new Insets(35, 43, 30, 5), 0, 0)
      layout += loadButton -> new Constraints(1, 1, 4, 2, 1, 1, NorthWest.id, Fill.None.id, new Insets(35, 185, 30, 5), 0, 0)
      layout += helpButton -> new Constraints(1, 1, 4, 2, 1, 1, NorthWest.id, Fill.None.id, new Insets(35, 328, 30, 5), 0, 0)
    }
    size = new Dimension(900, 600)
    resizable = false
  }

  //Button that loads a puzzle using the loadPuzzle method in IO. It pops up a dialog where the user can write the name of the file they want to load. If the load fails somehow,
  // a new dialog pops up explaining why the load fails, and the state of the current puzzle remains unchanged.
  val loadButton = new Button("Load puzzle") {
    preferredSize = new Dimension(120, 35)
    reactions += {
      case event.ButtonClicked(_) =>
        val fileName = JOptionPane.showInputDialog(null, "Please write the name of the file to be loaded and press OK.",
          "Load a puzzle", JOptionPane.PLAIN_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"), null, null)
          fileName match {
            case string: String => //If the user wrote something valid in the field:
              val potentialPuzzle = Game.GUI.IO.loadPuzzle(string)
              potentialPuzzle match {
                case puz: Puzzle =>
                  puzzle = puz
                  puzzle.allPiecesupdate
                  JOptionPane.showMessageDialog(null, "Load successsful!", "Save puzzle", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
                case string: String => //If an error occurs which provides some information on itself (The icon is just a little drawing I made as a sort of logo. It is the same in every dialog in the GUI):
                  JOptionPane.showMessageDialog(null, string, "Error", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
                case _ => //If some unexplained error occurs:
                  JOptionPane.showMessageDialog(null, "Unknown error.", "Error", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
              }
            case null => //If the user doesn't put anything in the field (nothing happens):
            case _ => //If the user puts something in the field which is not a String:
              JOptionPane.showMessageDialog(null, "Invalid input type.", "Error", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
          }
    }
  }

  //Button that saves the current puzzle to a file with the writePuzzle method in the IO. It pops up a dialog where the user can write the name of the file they want the
  // puzzle saved to. If the saving fails somehow, a new dialog pops up explaining why the save fails.
  val saveButton = new Button("Save Puzzle") {
    preferredSize = new Dimension(120, 35)
    reactions += {
      case event.ButtonClicked(_) =>
        val fileName = JOptionPane.showInputDialog(null, "Please write the name of the file you want the puzzle saved to and press OK.",
          "Save a puzzle", JOptionPane.PLAIN_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"), null, null)
           fileName match {
             case string: String => //If the user writes something valid in the field.
               Game.GUI.IO.writePuzzle(string, puzzle) match {
                 case unit: Unit => //If the saving was successful:
                   JOptionPane.showMessageDialog(null, "Save successsful!", "Save puzzle", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
                 case string: String => //If an error occurs which provides some information on itself:
                   JOptionPane.showMessageDialog(null, string, "Error", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
                 case _ => //If some unexplained error occurs:
                   JOptionPane.showMessageDialog(null, "Unknown error.", "Error", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
               }
             case null => //If the user doesn't put anything in the field (nothing happens):
             case _ => //If the user puts something in the field which is not a String:
               JOptionPane.showMessageDialog(null, "Invalid input type.", "Error", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
            }
    }
  }


  //Starts the current puzzle over (moves all the pieces back to the pile area):
  val startOverButton = new Button("Start over") {
    preferredSize = new Dimension(120, 35)
    reactions += {
      case event.ButtonClicked(_) =>
        puzzle.allPieces.foreach( _.changePos(Random.between(50, 440), Random.between(142, 508))) //Changes all the positions of the PuzzlePieces randomly to the pile area.
    }
  }

  //Button that provides a dialog with some instructions.
  val helpButton = new Button("Help") {
    preferredSize = new Dimension(120, 35)
    reactions += {
      case event.ButtonClicked(_) =>
        val helpMessage = "To solve the puzzle, all pieces need to be on the board, and each side of the pieces needs" +
          "\nto match up such that a bright color is next to the same color but lighter. For example, a" +
          "\nbright blue side needs to be next to a light blue side and a dark purple side next to a light" +
          "\npurple side. This is illustrated in the image that shows up in this pop up window." +
          "\n\nTo move a piece, drag it with your mouse. To put it into a slot on the board, drag it onto" +
          "\nthe slot. The piece will align itself into the slot as long as it is close enough to its center." +
          "\nTo rotate a piece, click it with your mouse." +
          "\n\nTo load an old puzzle, press the Load puzzle button and write the file name into the field." +
          "\nFor example \"./Game/ExampleFiles/SolvedPuzzle.txt\". You might need to click the screen" +
          "\nafterwards to make the loaded puzzle show up." +
          "\n\nTo save a puzzle to a file, click the Save puzzle button and write the name of the file you" +
          "\nwant to save to. For example \"Game/ExampleFiles/FileToBeWrittenTo.txt\"." +
          "\n\nTo start a new puzzle, press the Start new puzzle button." +
          "\nTo start the same puzzle again, press the Start over button." +
          "\n\nTo have the computer solve the puzzle for you, press Solve puzzle."
        //This dialog is the only onw with a different icon from the other dialohs. It shows an example to understand how to solve the puzzle.
        JOptionPane.showMessageDialog(null, helpMessage, "Help", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/InfoExample.png"))
    }
  }


  //Starts a new puzzle (technically the puzzle object stays the same, but all its pieces are changed to new ones):
  val startButton = new Button("Start new puzzle") {
    preferredSize = new Dimension(120, 35)
    reactions += {
      case event.ButtonClicked(_) =>
        puzzle.generatePieces //Generates a new set of PuzzlePieces (which removes the old pieces):
        puzzle.piecesInPile.foreach( _.changePos(Random.between(50, 440), Random.between(142, 508))) //Randomly places each piece into the pile area.
    }
  }

  //The main panel for the puzzle:
  class PuzzlePanel extends GridBagPanel {

    minimumSize = new Dimension(800, 500)
    preferredSize = new Dimension(900, 600)
    background = new Color(238, 234, 191)

    //Array of bounding triangles for the puzzle slots:
    // (The name alignment is because when a PuzzlePiece covers enough of the bounding triangle, it gets perfectly aligned into the triangle. This is better explainde by
    // actually playing the game.)
    var alignmentTriangles: Array[(Polygon, (Int, Int), String)] = Array()

    //Adds a bounding triangle for alignmentTriangles for each puzzle slot:
    // Every other piece gets a differently orientated triangle, which is why even and odd numbers are separated in the for loops.
    //First row:
    for (x <- 0 to 4) {
      if (x%2 == 0) {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(595 + (x/2)*60, 625 + (x/2)*60, 655 + (x/2)*60), Array(200, 115, 200), 3), (625 + (x/2)*60, 158), "up")
      }else {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(625 + (x/2)*60, 685 + (x/2)*60, 655 + (x/2)*60), Array(115, 115, 200), 3), (655 + (x/2)*60, 157), "down")
      }
    }

    //Second row:
    for (x <- 0 to 6) {
      if (x%2 == 0) {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(565 + (x/2)*60, 595 + (x/2)*60, 625 + (x/2)*60), Array(285, 200, 285), 3), (595 + (x/2)*60, 243), "up")
      }else {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(595 + (x/2)*60, 625 + (x/2)*60, 655 + (x/2)*60), Array(200, 285, 200), 3), (625 + (x/2)*60, 242), "down")
      }
    }

    //Third row:
    for (x <- 0 to 6) {
      if (x%2 == 0) {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(565 + (x/2)*60, 625 + (x/2)*60, 595 + (x/2)*60), Array(285, 285, 370), 3), (595 + (x/2)*60, 328), "down")
      }else {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(595 + (x/2)*60, 625 + (x/2)*60, 655 + (x/2)*60), Array(370, 285, 370), 3), (625 + (x/2)*60, 328), "up")
      }
    }

    //Fourth row:
    for (x <- 0 to 4) {
      if (x%2 == 0) {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(595 + (x/2)*60, 625 + (x/2)*60, 655 + (x/2)*60), Array(370, 455, 370), 3), (625 + (x/2)*60, 413), "down")
      }else {
        alignmentTriangles = alignmentTriangles :+ (new Polygon(Array(625 + (x/2)*60, 655 + (x/2)*60, 685 + (x/2)*60), Array(455, 370, 455), 3), (655 + (x/2)*60, 413), "up")
      }
    }

    override def paintComponent(g: Graphics2D) = {
      //Paints the pile area:
      g.setColor(new Color(215, 208, 133))
      val p = g.fillRoundRect(20, 100, 450, 450, 20, 20)

      //Paints the logo:
      g.setColor(Color.black)
      g.setFont(new Font("Rockwell", Font.BOLD, 45))
      g.drawString("Tri   ngle Puzzle", 500, 65)
      g.setColor(colorMap('b'))
      g.fillPolygon(Array(575, 589, 589), Array(65, 27, 53), 3)
      g.setColor(colorMap('A'))
      g.fillPolygon(Array(589, 589, 603), Array(53, 27, 65), 3)
      g.setColor(colorMap('D'))
      g.fillPolygon(Array(575, 589, 603), Array(65, 53, 65), 3)

      //Paints all the puzzle slots into the panel:
      // (The reason why this was not done with for loops as before is because it is the first thing I implemented in the GUI, so I had to use trial and error to figure out
      // where exactly each slot should go. All the previous for loops are based on the patterns of the slots below.)
      // First row
      g.setColor(Color.WHITE)
      g.fillPolygon(Array(595, 625, 655), Array(200, 115, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(595, 625, 655), Array(200, 115, 200), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(625, 685, 655), Array(115, 115, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(625, 685, 655), Array(115, 115, 200), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(655, 685, 715), Array(200, 115, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(655, 685, 715), Array(200, 115, 200), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(685, 745, 715), Array(115, 115, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(685, 745, 715), Array(115, 115, 200), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(715, 745, 775), Array(200, 115, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(715, 745, 775), Array(200, 115, 200), 3)

      // Second row
      g.setColor(Color.WHITE)
      g.fillPolygon(Array(565, 595, 625), Array(285, 200, 285), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(565, 595, 625), Array(285, 200, 285), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(595, 625, 655), Array(200, 285, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(595, 625, 655), Array(200, 285, 200), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(625, 655, 685), Array(285, 200, 285), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(625, 655, 685), Array(285, 200, 285), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(655, 685, 715), Array(200, 285, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(655, 685, 715), Array(200, 285, 200), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(685, 715, 745), Array(285, 200, 285), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(685, 715, 745), Array(285, 200, 285), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(715, 745, 775), Array(200, 285, 200), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(715, 745, 775), Array(200, 285, 200), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(745, 775, 805), Array(285, 200, 285), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(745, 775, 805), Array(285, 200, 285), 3)

      // Third row
      g.setColor(Color.WHITE)
      g.fillPolygon(Array(565, 625, 595), Array(285, 285, 370), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(565, 625, 595), Array(285, 285, 370), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(595, 625, 655), Array(370, 285, 370), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(595, 625, 655), Array(370, 285, 370), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(625, 655, 685), Array(285, 370, 285), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(625, 655, 685), Array(285, 370, 285), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(655, 685, 715), Array(370, 285, 370), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(655, 685, 715), Array(370, 285, 370), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(685, 715, 745), Array(285, 370, 285), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(685, 715, 745), Array(285, 370, 285), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(715, 745, 775), Array(370, 285, 370), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(715, 745, 775), Array(370, 285, 370), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(745, 775, 805), Array(285, 370, 285), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(745, 775, 805), Array(285, 370, 285), 3)

      // Fourth row
      g.setColor(Color.WHITE)
      g.fillPolygon(Array(595, 625, 655), Array(370, 455, 370), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(595, 625, 655), Array(370, 455, 370), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(625, 655, 685), Array(455, 370, 455), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(625, 655, 685), Array(455, 370, 455), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(655, 685, 715), Array(370, 455, 370), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(655, 685, 715), Array(370, 455, 370), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(685, 715, 745), Array(455, 370, 455), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(685, 715, 745), Array(455, 370, 455), 3)

      g.setColor(Color.WHITE)
      g.fillPolygon(Array(715, 745, 775), Array(370, 455, 370), 3)
      g.setColor(Color.BLACK)
      g.drawPolygon(Array(715, 745, 775), Array(370, 455, 370), 3)

      //From the above patterns, we can see that when the triangle is orientated upwards, the middle of the triangle is at y = ymin + 42, x = xmin + 30, and if the
      // orientation is downwards, the middle is at y = ymax - 42 and x = xmin + 30. This is what the coordinates of a PuzzlePiece and a puzzle slot correspond to.
      // This was adjusted a little for a couple slots to make them look a little nicer.

      //Paints all of the PuzzlePieces into the panel according to their current positions, rotation directions, and side orders:
      // Each PuzzlePiece consists of three triangles of different colors.
      for (x <- puzzle.allPieces.reverse) {
        if (x.rotationDir == "up") {
          g.setColor(colorMap(x.sides(0)))
          g.fillPolygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1), Array(x.pos._2 + 42, x.pos._2 - 42, x.pos._2 + 15), 3)
          g.setColor(colorMap(x.sides(1)))
          g.fillPolygon(Array(x.pos._1, x.pos._1, x.pos._1 + 30), Array(x.pos._2 + 15, x.pos._2 - 42, x.pos._2 + 42), 3)
          g.setColor(colorMap(x.sides(2)))
          g.fillPolygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1 + 30), Array(x.pos._2 + 42, x.pos._2 + 15, x.pos._2 + 42), 3)
        }else {
          g.setColor(colorMap(x.sides(0)))
          g.fillPolygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1), Array(x.pos._2 - 42, x.pos._2 + 42, x.pos._2 - 15), 3)
          g.setColor(colorMap(x.sides(1)))
          g.fillPolygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1 + 30), Array(x.pos._2 - 42, x.pos._2 - 15, x.pos._2 - 42), 3)
          g.setColor(colorMap(x.sides(2)))
          g.fillPolygon(Array(x.pos._1, x.pos._1, x.pos._1 + 30), Array(x.pos._2 - 15, x.pos._2 + 42, x.pos._2 - 42), 3)
        }
      }
    }


    //Listen to all the buttons and relevant mouse events:
    listenTo(mouse.moves)
    listenTo(mouse.clicks)
    listenTo(startButton)
    listenTo(loadButton)
    listenTo(saveButton)
    listenTo(startOverButton)
    listenTo(helpButton)

    //Keeps track of which piece needs to be aligned. when a piece is moved or rotated (dragged or clicked) it is saved here, and when the mouse is
    // released, if the alignment piece center is within a triangle in alignmnetTriangles, the piece is aligned perfectly into the triangle/slot.
    var alignmentPiece: Option[PuzzlePiece] = None

    //Keeps track of the piece which is being moved/dragged. When the mouse is pressed, if it is on a PuzzlePiece, the piece is saved here, and if a
    // mouse drag accompanies the press, this specific piece is also moved. Without this mechanism the mouse would pick up other pieces when being
    // dragged, and it would drop the piece if the mouse was moved too fast.
    var selectedPiece: Option[PuzzlePiece] = None

    //Keeps track of whether winning (solving the puzzle) was already announced, so that the "You solved it!" dialog doesn't keep popping up constantly after:
    var winAnnounced: Boolean = false

    reactions += {
      case MouseDragged(comp, point, a) =>
        //When the mouse is dragged the selectedPiece moves with it when the mouse is inside the top frame. The alignmentPiece becomes the selectedpiece so that it gets aligned when the mouse is released.
        // Since a piece was moved, winAnnounced becomes false.
        if (selectedPiece.isDefined) {if (point.x >= 0 && point.x <= 900 && point.y >= 0 && point.y <= 570) selectedPiece.get.changePos(point.x, point.y); puzzle.moveToFirstPlace(selectedPiece.get); winAnnounced = false}
        alignmentPiece = selectedPiece
        repaint()
      case MousePressed(comp, point, a, b, c) => //When the mouse is pressed if it is on a PuzzlePiece the alignmentPiece becomes the Puzzlepiece.
        selectedPiece = puzzle.allPieces.find( x =>
          if (x.rotationDir == "up") new Polygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1 + 30), Array(x.pos._2 + 42, x.pos._2 - 42, x.pos._2 + 42), 3)contains(point)
          else new Polygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1 + 30), Array(x.pos._2 - 42, x.pos._2 + 42, x.pos._2 - 42), 3)contains(point))
      case MouseClicked(c, point, mods, a, b) =>
        //When the mouse is clicked if it is on a PuzzlePiece the piece gets rotated. The alignment piece becomes the clicked piece.
        // Since a piece was moved, winAnnounced becomes false. A boardCheck and a repaint() is called so that the puzzle can be checked and a Dialog opened if it is solved.
        val moveablePiece = puzzle.allPieces.find( x =>
          if (x.rotationDir == "up") new Polygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1 + 30), Array(x.pos._2 + 42, x.pos._2 - 42, x.pos._2 + 42), 3)contains(point)
          else new Polygon(Array(x.pos._1 - 30, x.pos._1, x.pos._1 + 30), Array(x.pos._2 - 42, x.pos._2 + 42, x.pos._2 - 42), 3)contains(point))
        if (moveablePiece.isDefined) {
          moveablePiece.get.rotateRight
          winAnnounced = false
        }
        alignmentPiece = moveablePiece
        puzzle.boardCheck
        repaint()
        if (puzzle.isSolved && !winAnnounced) {
          JOptionPane.showMessageDialog(null, "You solved it!", "Solver", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
          winAnnounced = true //Once the win has been announced in a dialog for a specific puzzle configuration, winAnnounced is set to true.
        }
        repaint()
      case mouseReleased: MouseReleased =>
        //When the mouse is released the alignmentPiece is aligned into the slot that it is on. This needs to be done after the mouse is released because doing it
        // before would cause the piece to stay stuck in the same slot for the rest of the game, and only slots on the edges of the board could be accessed.
        // The board is checked and then whether the puzzle is solved is checked.
        if (alignmentPiece.isDefined) {
          val selectedTriangle = alignmentTriangles.find( x => x._3 == alignmentPiece.get.rotationDir && x._1.contains(new Point(alignmentPiece.get.pos._1, alignmentPiece.get.pos._2)))
          if (selectedTriangle.isDefined) {alignmentPiece.get.changePos(selectedTriangle.get._2._1, selectedTriangle.get._2._2)}
        }
        puzzle.boardCheck
        repaint()
        if (puzzle.isSolved && !winAnnounced) {
          JOptionPane.showMessageDialog(null, "You solved it!", "Solver", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
          winAnnounced = true //Once the win has been announced in a dialog for a specific puzzle configuration, winAnnounced is set to true.
        }
        repaint()
      case buttonClicked: ButtonClicked =>
        //When a button is clicked a boardCheck is called and whether the puzzle is solved is checked. A repaint() is called.
        puzzle.boardCheck
        repaint()
        if (puzzle.isSolved && !winAnnounced) {
          JOptionPane.showMessageDialog(null, "You solved it!", "Solver", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("./Game/GUIFiles/TrianglePuzzleLogo.png"))
          winAnnounced = true //Once the win has been announced in a dialog for a specific puzzle configuration, winAnnounced is set to true.
        }
        repaint()
    }

  }

}
