package fpinscala.structuringprograms

case class Box(height: Double, width: Double)

object Exercises {

  def greaterBy(x: Box, y: Box, f: Box => Double): Box = 
  if (f(x) > f(y)) x else y

  def wider(x: Box, y: Box): Box =
    greaterBy(x, y, _.width)

  def taller(x: Box, y: Box) =
    greaterBy(x, y, _.height)

  type Pred[A] = A => Boolean

  def absolute[A](f: A => Int): A => Int = n => f(n).abs

  def divisibleBy(k: Int): Pred[Int] = _ % k == 0

  val even = divisibleBy(2)

  val divisibleBy3And5: Pred[Int] = lift(_ && _, divisibleBy(3), divisibleBy(5))

  val divisibleBy3Or5: Pred[Int] = lift(_ || _, divisibleBy(3), divisibleBy(5))

  def lift[A](f: (Boolean, Boolean) => Boolean,
              g: Pred[A],
              h: Pred[A]): Pred[A] = n => f(g(n), h(n))

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = sys.error("todo")

  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B,
                                          h: A => C,
                                          i: A => D): A => E = sys.error("todo")

  def fib(n: Int): Int = sys.error("todo")

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n // We want to find the `x` such that `x` squared minus `n` equals `0`.
    iterateWhile(2.0)(x => x - f(x) / (2 * x), // Starting with a guess of `2.0`, iteratively improve the guess.
                      x => f(x).abs > 1e-14) // `1e-14` is a way of writing `10` to the `-14`th power, a rather small number. When the difference between the guess and the answer is smaller than this, the guess is "good enough".
  }
  
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = sys.error("todo")
}