package webapp

class Model {
  private var state: (Game, Boolean) = (Game.someTable(), false)

  def getState(): (Game, Boolean) = state

  def shift(direction: Direction): Unit = {
    val st = state._1.shift(direction).getOrElse(state._1)
    //TODO:
    state = (st, false)
  }

}
