package algebraic_data_types

final case class Board(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card)
    extends Cards {
  def cards(): List[Card] = List(c1, c2, c3, c4, c5)
}
