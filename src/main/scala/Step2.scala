import Functions._

/** multiple evaluation. Manually */
object Step2 extends App {
  val (fRes, fStr) = f(3)
  val (gRes, gStr) = g(fRes)
  val (hRes, hStr) = h(gRes)

  val log = fStr + gStr + hStr

  println(s"final result $hRes")
  println(s"results ${log}")
}

/** multiple evaluation with @bind function (easier to add more) */
object Step3 extends App {
  val (fResult, fLog) = f(3)
  val (gResult, gLog) = bind(g, (fResult, fLog))
  val (hResult, hLog) = bind(h, (gResult, gLog))

  val (result, log) = bind(h, bind(g, f(3)))

  println(s"final result $result")
  println(s"results ${log}")
}

/** using for comprehension (moving logic to Wrapper flatMap) */
object Step4 extends App {
  val result = for {
    fResult <- Wrapper(f(3))
    gResult <- Wrapper(g(fResult))
    hResult <- Wrapper(h(gResult))
  } yield hResult


  println(s"final result ${result.value}")
  println(s"results ${result.log}")
}
object Wrapper {
  def apply(value: Int, log: String): Wrapper = new Wrapper(value, log)

  def apply(tuple2: (Int, String)): Wrapper = {
    new Wrapper(tuple2._1, tuple2._2)
  }
}

case class Wrapper private(value: Int, log: String) {

  def map(f: Int => Int): Wrapper = {
    val result = f(value)
    Wrapper(result, log)
  }

  def flatMap(f: Int => Wrapper): Wrapper = {
    val result = f(value)
    Wrapper(result.value, result.log + log)
  }

}

object Functions {
  def bind(f: Int => (Int, String),
           acc: (Int, String)
          ): (Int, String) = {
    val (fRes, fStr) = f(acc._1)
    val str = acc._2 + fStr
    (fRes, str)
  }

  def f(a: Int): (Int, String) = {
    val result = a * 2
    (result, s"\nf result = $result")
  }

  def g(a: Int): (Int, String) = {
    val result = a * 3
    (result, s"\ng result = $result")
  }

  def h(a: Int): (Int, String) = {
    val result = a * 4
    (result, s"\ng result = $result")
  }
}