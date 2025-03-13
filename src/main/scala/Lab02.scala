object Lab02 extends App:

  // Task 1, Svolto con Giacomo Foschi (prima consegna)
  val hello: String = "Hello, Scala"

  println(hello)

  val sumFun: (Int, Int) => Int = _ + _
  sumFun(3, 4)

  def sumMeth(x: Int, y: Int): Int = x + y
  def currieSum(x: Int)(y: Int): Int = x + y

  println(sumMeth(10, 12))
  println(currieSum(10)(sumMeth(12, 20)))

  val increment = currieSum(1)
  currieSum(increment(1))(1)

  val inc: Int => Int = _ + 1
  currieSum(inc(1))(1)

  val currieIncrement: Int => Int = currieSum(1)

  println(currieIncrement(2))

  val curriedSumAsFun: Int => Int => Int = x => y => x + y
  curriedSumAsFun(10)(20)

  //Task 2, Svolto da solo

  //Function literal
  val positive: Int => String = _ match
    case x if x>=0 => "positive"
    case x if x<0 => "negative"

  println(positive(2))
  println(positive(-2))
  println(positive(0))

  //Method syntax
  def pos(x: Int): String = x match
    case x if x>=0 => "positive"
    case x if x<0 => "negative"

  println(pos(2))
  println(pos(-2))
  println(pos(0))

  //Neg function
  def neg(f:String=>Boolean): String => Boolean = v => !f(v)
  val empty: String => Boolean = _ == ""
  val f = neg(empty)("c")

  println(f)

  //Neg whit generics
  def negGen[X](f:X=>Boolean): X => Boolean = v => !f(v)
  val isZero: Int => Boolean = _ == 0
  val fGen = negGen(isZero)(2)

  println(fGen)

  //Currying
  //val p1: <CurriedFunType>:
  val p1: Int => Int => Boolean => Boolean = x => y => z => (x <= y) == z

  println(p1(2)(3)(true))
  println(p1(3)(2)(true))
  println(p1(3)(2)(false))
  //val p2: <NonCurriedFunType>:
  val p2: (Int, Int, Boolean) => Boolean = _<=_ == _

  println(p2(2, 3, true))
  println(p2(3, 2, true))
  println(p2(3, 2, false))
  //def p3(...)(...)(...): ... = ...:
  def p3(x: Int)(y: Int)(z: Boolean): Boolean = x <= y == z

  println(p3(2)(3)(true))
  println(p3(2)(3)(false))
  println(p3(3)(2)(false))
  println(p3(3)(2)(true))
  //def p4(...): ... = ...:
  def p4(x:Int, y:Int, z:Boolean): Boolean = x <= y == z

  println(p4(3, 2, true))
  println(p4(3, 2, false))
  println(p4(2, 3, true))

  //Functional compositions
  val f1 = (x: Int) => x + 1
  val g = (x: Int) => x * 2
  def composeFun(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  println(composeFun(f1, g)(5))
  //Generics:
  def composeFunGen[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))
  val addOne: Int => Double = _ + 1.0
  val timesTwo: Double => String = x => s"Result: ${x * 2}"

  println(composeFunGen(timesTwo, addOne)(5))

  //Functional compositions with 3 arbitrary functions
  def compose3Fun[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D = x => f(g(h(x)))
  val mul3: Double => Double = _ * 3.0
  val composed = compose3Fun(timesTwo, mul3, addOne)

  println(composed(5))
  println(compose3Fun(timesTwo, mul3, addOne)(5))

  //Task 3, Svolto da solo
  //Recursive function for power of a number
  //Standard recursion:
  def power(base: Double, exp: Int): Double = exp match
      case 0 => 1
      case n if n > 0 => base * power(base, exp - 1)

  println(power(2, 0))
  println(power(2, 2))
  println(power(1000, 2))
  //Tail recursion:
  def powerTail(base: Double, exponent: Int): Double =
      @annotation.tailrec
      def _power(exp: Int, acc: Double): Double = exp match
          case 0 => acc
          case _ => _power(exp - 1, acc * base)
      _power(exponent, 1)

  println(powerTail(10, 2))
  println(powerTail(50, 2))
  println(powerTail(2, 8))
  //Function to reverse the digits
  def reverseDigit(n: Int): Int =
    @annotation.tailrec
    def _reverse(n: Int, acc: Int): Int = n match
        case 0 => acc
        case _ => _reverse(n / 10, acc * 10 + n % 10)
    _reverse(n, 0)

  println(reverseDigit(1234))
  println(reverseDigit(167531))
  println(reverseDigit(1))

  //Task 4, Svolto da solo
  //Test: https://github.com/GiovanniPisoni/pps-lab02/blob/main/src/test/scala/ArithmeticExpressionsTest.scala
  //Sum type to represent arithmetic expressions
  enum Expr:
    case Literal (v: Int)
    case Add (x: Expr, y: Expr)
    case Multiply (x: Expr, y: Expr)

  object Expr:
    def evaluate(expr: Expr): Int = expr match
        case Expr.Literal(v) => v
        case Expr.Add(x, y) => evaluate(x) + evaluate(y)
        case Expr.Multiply(x, y) => evaluate(x) * evaluate(y)

    def show(expr: Expr): String = expr match
      case Expr.Literal(v) => s"$v"
      case Expr.Add(x, y) => "(" + show(x) + " + " + show(y) + ")"
      case Expr.Multiply(x, y) => "(" + show(x) + " * " + show(y) + ")"

  import Expr.*

  val literal = evaluate(Expr.Literal(1))
  val add = Expr.Add(Expr.Literal(1),Expr.Literal(2))
  val evaluateAdd = evaluate(add)
  val showAdd = show(add)
  val mul = Expr.Multiply(Expr.Literal(1),Expr.Literal(2))
  val evaluateMul = evaluate(mul)
  val showMul = show(mul)

  println("Expr Literal: " + show(Expr.Literal(literal)))
  println(s"Expr Add of: $showAdd Result: $evaluateAdd")
  println(s"Expr Multiply of: $showMul Result: $evaluateMul")

  //Task 5, Svolto da solo
  //Code: https://github.com/GiovanniPisoni/pps-lab02/blob/main/src/main/scala/task5/Optional.scala
  //Test: https://github.com/GiovanniPisoni/pps-lab02/blob/main/src/test/scala/task5/OptionalTest.scala