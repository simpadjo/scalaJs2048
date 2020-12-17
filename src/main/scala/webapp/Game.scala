package webapp

import scala.util.Random

case class Game(cells: Board, score: Int) {

  def shift(dir: Direction): Option[Game] = {
    val (rotation, backRotation) = Rotations.chooseRotations(dir)

    val rotated = rotation(cells)

    val res = Game(rotated, -1).shiftLeft().map(g => {
      val andBack = backRotation(g.cells)
      Game(andBack, g.score)
    })

    res
  }

  def shiftLeft(): Option[Game] = {
    val res = cells.map(row => {
      val (shifted, points) = ShiftUtil.shiftLeft(row)
      val padded = shifted.map(Some(_)).padTo(boardSize, None)
      val changed = (padded != row)

      (ShiftUtil.addNewCellIfCan(padded), changed, points)
    })

    if (res.exists(_._2)) {
      val totalpoints = score + res.map(_._3).sum
      Some(Game(res.map(_._1), totalpoints))
    } else None
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

  def empty(): Game = {
    Game(Seq.tabulate(4)(i => Seq.tabulate(4)(j => None)), 0)
  }


  def someTable(): Game = {
    Game(Seq.tabulate(4)(i => Seq(Some(2), None, Some(2), Some(4))), 0)
  }


  def nextNum(): Int = {
    if (rand.nextBoolean()) 4 else 2
  }

}
