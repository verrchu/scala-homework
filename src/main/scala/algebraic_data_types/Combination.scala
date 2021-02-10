package algebraic_data_types

// Notice Combination contains Variant which consists of Combination's cards
sealed abstract class Combination(variant: Variant) extends Cards {
  override def cards(): List[Card] = variant.cards()
}
object Combination {
  final case class StraightFlush(high: Rank, variant: Variant)
      extends Combination(variant)
  final case class FourOfAKind(rank: Rank, rest: (Card), variant: Variant)
      extends Combination(variant)
  final case class FullHouse(two: Rank, three: Rank, variant: Variant)
      extends Combination(variant)
  final case class Flush(rank: Rank, variant: Variant)
      extends Combination(variant)
  final case class Straight(high: Rank, variant: Variant)
      extends Combination(variant)
  final case class ThreeOfAKind(
      rank: Rank,
      rest: (Card, Card),
      variant: Variant
  ) extends Combination(variant)
  final case class TwoPairs(
      low: Rank,
      high: Rank,
      rest: (Card),
      variant: Variant
  ) extends Combination(variant)
  final case class Pair(rank: Rank, rest: (Card, Card, Card), variant: Variant)
      extends Combination(variant)
  final case class HighCard(
      rest: (Card, Card, Card, Card, Card),
      variant: Variant
  ) extends Combination(variant)
}
