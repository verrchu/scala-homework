package algebraic_data_types

final case class Card(rank: Rank, suit: Suit)

// allows arbitrary structure to be represented as list of cards
trait Cards {
  def cards(): List[Card]
}
