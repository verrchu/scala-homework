package algebraic_data_types

sealed abstract class Combination
object Combination {
  final case class StraightFlush(high: Rank) extends Combination
  final case class FourOfAKind(rank: Rank, rest: (Rank)) extends Combination
  final case class FullHouse(two: Rank, three: Rank) extends Combination
  final case class Flush(rank: Rank) extends Combination
  final case class Straight(high: Rank) extends Combination
  final case class ThreeOfAKind(rank: Rank, rest: (Rank, Rank))
      extends Combination
  final case class TwoPairs(low: Rank, high: Rank, rest: (Rank))
      extends Combination
  final case class Pair(rank: Rank, rest: (Rank, Rank, Rank))
      extends Combination
  final case class HighCard(rest: (Rank, Rank, Rank, Rank, Rank))
      extends Combination
}
