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