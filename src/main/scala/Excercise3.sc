import scala.util.Random

val a = List(1,2,3)
for (x <- a)
  print(x)

def random(x : Int) = Random.nextInt(x)

/**Write snippet that sets a to an array of random integers
 * between 0 (inclusive) and n (exclusive)*/
def generate_random(n : Int) : Iterable[Int] = {
  n match {
    case x if x > 0         => List.range(0, n) map (f => random(n))
    case _                  => Seq.empty
  }
  //TODO: Recursively
}

print("Type the number to randomize : ")
val n =10
print(generate_random(n))

