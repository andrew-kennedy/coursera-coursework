def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1 else f(a) * product(f)(a + 1, b)
}

def factorial(n: Int): Int = {
  product(x => x)(1, n)
}

factorial(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int = {
  if (a > b) unit
  else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))
}

mapReduce(x => x, (y, z) => y + z, 0)(1, 4)

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  Math.abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    println(guess)
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
fixedPoint(x => 1 + x / 2)(2.1)
def squareRoot(x: Double) = fixedPoint(averageDamp(y => x / y))(1)

squareRoot(2)



