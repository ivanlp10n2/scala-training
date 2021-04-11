import scala.annotation.tailrec

object FootballGame extends App {

  sealed trait Movement
  case object Goal_A extends Movement
  case object Goal_B extends Movement
  case object Quit extends Movement
  case object Nothing extends Movement

  case class ApplicationError(description: String)

  sealed trait Team
  case class A(goals: Int) extends Team
  case class B(goals: Int) extends Team

  case class Game(ATeam: Int, BTeam: Int)

  def askMovement: Unit = {
    print(
      "1) Goal A \n" +
        "2) Goal B \n" +
        "3) End Game")
  }

  def readMovement: Movement =
    scala.io.StdIn.readInt() match {
      case 1 => Goal_A
      case 2 => Goal_B
      case 3 => Quit
      case _ => Nothing
    }

  def evaluateMovement(movement: Movement, game: Game): Game = movement match{
    case Goal_A => game.copy(ATeam = game.ATeam + 1)
    case Goal_B => game.copy(BTeam = game.BTeam + 1)
    case _ => game
  }

  @tailrec
  def playGame(game: Game): Game = {
    askMovement
    readMovement match {
      case Quit => game
      case movement => playGame(evaluateMovement(movement, game))
    }
  }

  var freshGame = Game(0,0)
  print("Welcome to football game \n")
  val gameState = playGame(freshGame)
  print(s"State of the game: $gameState")

}

object Functions {

  def timer[A](f: => A): (A, Long) = {
    val start = System.currentTimeMillis()
    val function = f
    val end = System.currentTimeMillis()
    (function, end - start)
  }

  def someLongRunningFunction: Int = {
    Thread.sleep(33); 3
  }
}
