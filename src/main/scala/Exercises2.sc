import worksheet._

/** returns sigmoid function based on number sign
 * @param num used to check sign */
def sigmoid(num: Int) =
  num match {
    case x if x > 0   =>  1
    case x if x < 0   => -1
    case x if x == 0  =>  0
  }

test( sigmoid(4) == 1)
test( sigmoid(-3) == -1)
test(sigmoid(0) == 0)

/** value of {} = ()
 * type of = Unit */
val t = {}
print(t)


/** Equivalent for scala */
var b = 10
for (i <- (1 to 10).reverse) print(i)


// for loop execution with a yield
var retVal = for{ a <- 0 to 10 if a != 3; if a < 8 }yield a

import scala.language.postfixOps
/**Produce countdown(n : Int) that prints numbers from n to 0 */
def countdown(value: Int) = (1 to 10).reverse toList

test(countdown(10) == List(10,9,8,7,6,5,4,3,2,1))

/**Computes product of Unicode values from a String*/
def unicode_product : String => Long =
  (word : String) => word.toList map (_.toInt) map (_.toLong) reduce (_*_)

unicode_product("Hello")

import scala.annotation.tailrec
/**recursive unicode_product*/
def recursive_unicode_function(word : => String ) : Long = {
  @tailrec
  def product_unicode( word : List[Char], accumulator : Long = 1L) : Long = {
    word match {
      case Nil => accumulator
      case x :: tail => product_unicode(tail, accumulator * x.toInt.toLong)
    }
  }
  product_unicode(word.toList)
}
recursive_unicode_function("Hello")

val testWord = "Hello"
test (recursive_unicode_function(testWord) == unicode_product(testWord))


/**Evaluates x ^ n recursively with the following rules
 * - x ^ n = y if n is even and positive, where y = x ^ (n / 2)
 * - x ^ n = x * x ^ -1^
 * @param n is Integer
 * */
def compute_pow (base : BigDecimal, exponential : Int)  = 3


