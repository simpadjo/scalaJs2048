package webapp

import org.scalajs.dom
import org.scalajs.dom.ext.KeyValue.{ArrowDown, ArrowLeft, ArrowRight, ArrowUp}
import org.scalajs.dom.raw.Document
import org.scalajs.dom.{Element, document}
import webapp.{Game, Model}

import scala.scalajs.js.annotation.JSExportTopLevel

object TutorialApp {
  def main(args: Array[String]): Unit = {
    val model = new Model()

    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      setupUI(model)
    })
  }

  def setupUI(model: Model): Unit = {
    val table = document.createElement("div")
    document.onkeydown =
      e => {
        onKey(document, model,table, e.key)
      }

    document.body.appendChild(table)

  }

  def onKey(document: Document, model: Model, parent: Element, key: String): Unit = {
    val arrow: Option[Direction] = key match {
      case ArrowUp => Some(Up)
      case ArrowDown => Some(Down)
      case ArrowLeft => Some(Left)
      case ArrowRight => Some(Right)
      case _ => None
    }

    arrow.foreach(dir => {
      model.shift(dir)
      val st = model.getState()
      val newTable = render(document, st._1)
      val prev = parent.firstElementChild
      if (prev != null) {
        parent.removeChild(prev)
      }
      parent.appendChild(newTable)
    }
    )
  }

  def render(document: Document, game: Game): Element = {
    val res = document.createElement("table")
    game.cells.foreach(row => {
      val rowEl = document.createElement("tr")
      row.foreach(cell => {
        val cellEl = document.createElement("th")
        cellEl.textContent = cell.fold("-")(_.toString)
        rowEl.appendChild(cellEl)
      })
      res.appendChild(rowEl)
    })
    res
  }
}