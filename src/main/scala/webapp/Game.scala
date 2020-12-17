package webapp

import webapp.Game.size

import scala.util.Random

case class Game(cells: Seq[Row], score: Int){

  def shift(dir: Direction): Option[Game] = {
    val rotated = dir match {
      case Up => Rotations.deg90(cells)
      case Down =>  Rotations.minus90(cells)
      case Left => cells
      case Right => Rotations.deg180(cells)
    }

    val res = Game(rotated, -1).shiftLeft().map(g => {
      val back = dir match {
        case Up => Rotations.deg90(g.cells)
        case Down =>  Rotations.minus90back(g.cells)
        case Left => g.cells
        case Right => Rotations.deg180(g.cells)
      }
      Game(back, g.score)
    })

    res
  }

  def shiftLeft(): Option[Game] = {
    val res = cells.map(row => {
      val (shifted, points) = ShiftUtil.shiftLeft(row)
      val padded = shifted.map(Some(_)).padTo(size, None)
      val changed = (padded != row)

      (ShiftUtil.addNewCellIfCan(padded), changed, points)
    })

    if(res.exists(_._2)){
      val totalpoints = score + res.map(_._3).sum
      Some(Game(res.map(_._1), totalpoints))
    } else None
  }
}

object Rotations{
  def deg90(rows: Seq[Row]): Seq[Row] = rows.transpose
  def deg180(rows: Seq[Row]): Seq[Row] = rows.map(_.reverse)
  def minus90(rows: Seq[Row]): Seq[Row] = rows.transpose.map(_.reverse)
  def minus90back(rows: Seq[Row]): Seq[Row] = rows.map(_.reverse).transpose
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
    if(rand.nextBoolean()) 4 else 2
  }

  val size: Int = 4


}
