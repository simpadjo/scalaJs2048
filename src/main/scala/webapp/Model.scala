package webapp

class Model {
  private var state: (Game, MoveResult) = (Game.someTable(), NoEffect)

  def getState(): (Game, MoveResult) = state

  def shift(direction: Direction): Unit = {
    state = state._1.tryShift(direction) match {
      case m @ Moved(newTable, finished) => (newTable, m)
      case NoEffect => (state._1, NoEffect)
    }
  }

}
