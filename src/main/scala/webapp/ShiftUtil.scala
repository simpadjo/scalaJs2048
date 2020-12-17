package webapp

import scala.annotation.tailrec

object ShiftUtil {
  def shiftLeft(row: Row): (Seq[Int], Int) = {
    @tailrec
    def go(steady: Seq[Int], squashable: Option[Int], rest: Row, points: Int): (Seq[Int], Int) = {
      if(rest.isEmpty){
        (steady ++ squashable, points)
      } else {
        rest.head match {
          case Some(nxt) => {
            squashable match {
              case Some(v) if v == nxt => {
                val squashed = nxt * 2
                go(steady :+ squashed, None, rest.tail, points + squashed)
              }
              case Some(v) =>   go(steady :+ v, Some(nxt), rest.tail, points)
              case None =>go(steady, Some(nxt), rest.tail, points)
            }
          }
          case None =>go(steady, squashable, rest.tail, points)
        }
      }
    }

    go(Nil, None, row, 0)
  }

  def addNewCellIfCan(row: Row): Row = {
    if(row.last.isEmpty)
      row.dropRight(1) :+ Some(Game.nextNum())
    else
      row
  }

  //TODO: factor-out randomization to make it testable
  def shiftLeft(cells: Board): Option[(Board, Int)] = {
    val res = cells.map(row => {
      val (shifted, points) = ShiftUtil.shiftLeft(row)
      val padded = shifted.map(Some(_)).padTo(boardSize, None)
      val changed = (padded != row)

      (ShiftUtil.addNewCellIfCan(padded), changed, points)
    })

    if (res.exists(_._2)) {
      val totalpoints = res.map(_._3).sum
      Some((res.map(_._1), totalpoints))
    } else None
  }

}
