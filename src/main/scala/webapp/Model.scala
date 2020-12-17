package webapp

case class Model(game: Game, lost: Boolean) {
  def shift(direction: Direction): Option[Model] = {
    if (lost)
      None
    else
      game.tryShift(direction) match {
        case Moved(newTable, finished) => Some(Model(newTable, finished))
        case NoEffect => None
      }
  }

}
