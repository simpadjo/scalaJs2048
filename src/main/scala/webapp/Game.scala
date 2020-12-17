package webapp

import scala.util.Random

case class Game(cells: Board, score: Int) {

  def shift(dir: Direction): Option[Game] = {
    val (rotation, backRotation) = Rotations.chooseRotations(dir)

    val rotated = rotation(cells)

    ShiftUtil.shiftLeft(rotated).map{ case (newBoard, plusScore) => {
      val andBack = backRotation(newBoard)
      Game(andBack, score + plusScore)
    }}
  }
}

object Rotations {
  type Rotation = Board => Board

  val rightRotation: Rotation = b => b.transpose
  val reverseRightRotation: Rotation = rightRotation

  val leftRotation: Rotation = b => b.transpose.map(_.reverse)
  val reverseLeftRotation: Rotation = b => b.map(_.reverse).transpose

  val revert: Rotation = b => b.map(_.reverse)

  def chooseRotations(direction: Direction): (Rotation, Rotation) = {
    direction match {
      case Up => (rightRotation, reverseRightRotation)
      case Down => (leftRotation, reverseLeftRotation)
      case Left => (identity, identity)
      case Right => (revert, revert)
    }
  }
}

sealed trait Direction

case object Up extends Direction

case object Down extends Direction

case object Left extends Direction

case object Right extends Direction

object Game {
  private val rand = new Random()

  def someTable(): Game = {
    val row1 = Random.shuffle(Seq(Some(nextNum()), None, None, None))
    val row2 = Random.shuffle(Seq(Some(nextNum()), None, None, None))
    val emptyRow = Seq.tabulate(boardSize)(_ => None)
    val rows = Seq(row1, row2, emptyRow, emptyRow)
    Game(Random.shuffle(rows), 0)
  }


  def nextNum(): Int = {
    if (rand.nextBoolean()) 4 else 2
  }

}
