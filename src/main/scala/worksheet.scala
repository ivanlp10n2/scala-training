package object worksheet {
  var messages = List.empty[String]

  def print(x: Any): Unit = {
    messages = x.toString :: messages
    Console.print(x)
  }

  def test(expr: => Boolean): Unit = {
    expr match {
      case true => this.print(s"assert OK")
      case _ => println("====Wrong assertion=== ")
    }
  }
}
