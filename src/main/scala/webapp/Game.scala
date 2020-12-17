package webapp

import webapp.Game.addNewCell

import scala.util.Random

sealed trait MoveResult
case class Moved(newTable: Game, finished: Boolean) extends MoveResult
object NoEffect extends MoveResult

case class Game(cells: Board, score: Int) {

  def tryShift(dir: Direction): MoveResult = {
    ShiftUtil.shift(cells, dir) match {
      case Some((newBoard, plusScore)) => {
        val withNextCell = addNewCell(newBoard)
        val newScore = score + plusScore
        val isFinished = Seq(Up, Down, Left, Right)
          .forall(d => ShiftUtil.shift(withNextCell, d).isEmpty)
        Moved(Game(withNextCell, newScore), isFinished)
      }
      case None => NoEffect
    }
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

  def addNewCell(board: Board): Board = {
    val emptyPositions: Seq[(Int, Int)] = board.zipWithIndex.flatMap{
      case (row, i) => {
        row.zipWithIndex.filter(_._1.isEmpty).map(e => (i, e._2))
      }
    }
    require(emptyPositions.nonEmpty)
    val (i, j) = rand.shuffle(emptyPositions).head
    val v = Some(nextNum())
    board.updated(i, board(i).updated(j, v))
  }

}
