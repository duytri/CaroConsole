package uit.islab.ai

sealed abstract class Player
case object X extends Player
case object O extends Player
case object Blank extends Player {
    override def toString = " "
}