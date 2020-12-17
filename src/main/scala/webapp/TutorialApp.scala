package webapp

import org.scalajs.dom
import org.scalajs.dom.ext.KeyValue.{ArrowDown, ArrowLeft, ArrowRight, ArrowUp}
import org.scalajs.dom.raw.Document
import org.scalajs.dom.{Element, document}

case class Views(table: Element, score: Element, lost: Element)

object TutorialApp {
  var model = Model(Game.someTable(), false)

  def main(args: Array[String]): Unit = {

    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      setupUI()
    })
  }

  def setupUI(): Unit = {
    val views = Views(
      document.createElement("div")
      , document.createElement("div")
      , document.createElement("div")
    )

    views.score.setAttribute("height", "40")
    views.lost.setAttribute("height", "40")

    render(document, model, views)

    document.onkeydown = { e => {
      onKey(document, views, e.key)
    }
    }

    document.body.appendChild(views.table)
    document.body.appendChild(views.score)
    document.body.appendChild(views.lost)

  }

  def onKey(document: Document, views: Views, key: String): Unit = {
    val arrow: Option[Direction] = key match {
      case ArrowUp => Some(Up)
      case ArrowDown => Some(Down)
      case ArrowLeft => Some(Left)
      case ArrowRight => Some(Right)
      case _ => None
    }

    arrow.foreach(dir => {
      model.shift(dir).foreach(newModel => {
        model = newModel
        render(document, model, views)
      })
    }
    )
  }

  def render(document: Document, m: Model, views: Views): Unit = {
    val game = m.game
    val newTable = document.createElement("table")
    game.cells.foreach(row => {
      val rowEl = document.createElement("tr")
      row.foreach(cell => {
        val cellEl = document.createElement("th")
        cellEl.setAttribute("width", "20")
        cellEl.setAttribute("height", "20")
        cellEl.textContent = cell.fold("_")(_.toString).padTo(4, ' ')
        rowEl.appendChild(cellEl)
      })
      newTable.appendChild(rowEl)
    })

    val prev = views.table.firstElementChild
    if (prev != null) {
      views.table.removeChild(prev)
    }
    views.table.appendChild(newTable)
    views.score.textContent = s"Score: ${model.game.score.toString}"
    if (model.lost) {
      views.lost.textContent = "lost!"
    }


  }
}