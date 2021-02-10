package algebraic_data_types

sealed trait Hand extends Cards
object Hand {
  // Texas Holdem
  final case class TH(c1: Card, c2: Card) extends Hand {
    def cards(): List[Card] = List(c1, c2)
  }
  // Omaha Holdem
  final case class OH(c1: Card, c2: Card, c3: Card, c4: Card) extends Hand {
    def cards(): List[Card] = List(c1, c2, c3, c4)
  }
}
