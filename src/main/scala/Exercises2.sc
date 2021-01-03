import scala.language.postfixOps

/** returns sigmoid function based on number sign
 * @param num used to check sign */
def sigmoid(num: Int) =
  num match {
    case x if x > 0   =>  1
    case x if x < 0   => -1
    case x if x == 0  =>  0
  }

assert( sigmoid(4) == 1)
assert( sigmoid(-3) == -1)
assert(sigmoid(0) == 0)

/** value of {} = ()
 * type of = Unit */
val t = {}
print(t)


/** Equivalent for scala */
var b = 10
for (i <- (1 to 10).reverse) print(i)


// for loop execution with a yield
var retVal = for{ a <- 0 to 10 if a != 3; if a < 8 }yield a

/**Produce countdown(n : Int) that prints numbers from n to 0 */
def countdown(value: Int) = (1 to 10).reverse toList

assert(countdown(10) == List(10,9,8,7,6,5,4,3,2,1))


