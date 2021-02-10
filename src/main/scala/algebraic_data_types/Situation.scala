package algebraic_data_types

abstract class Situation(board: Board, hands: List[Hand])
object Situation {
  // Texas Holdem
  final case class TH(board: Board, hands: List[Hand.TH])
      extends Situation(board, hands)
  // Omaha Holdem
  final case class OH(board: Board, hands: List[Hand.OH])
      extends Situation(board, hands)
}
