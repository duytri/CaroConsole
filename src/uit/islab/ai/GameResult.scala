package uit.islab.ai

object GameResult extends Enumeration {
    type GameResult = Value
    val X, O, Tie, NoResult = Value

    def displayGameResult(gameResult: GameResult) : String = {
        val winnerText = "Congratulation, Player %s won!"

        gameResult match {
            case GameResult.NoResult => "No winner yet!"
            case GameResult.Tie => "It's a tie!"
            case player => winnerText.format(player)
        }
    }
}