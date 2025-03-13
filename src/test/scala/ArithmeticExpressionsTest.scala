import Lab02.Expr
import org.junit.*
import org.junit.Assert.*

class ArithmeticExpressionsTest:
  private val literalValue = 1
  private val literalValue2 = 2

  @Test def testCreationOfLiteralExpr(): Unit =
        assertNotNull(Expr.Literal(literalValue))

  @Test def testCreationOfAddExpr(): Unit =
        val value = Expr.Literal(literalValue2)
        assertNotNull(Expr.Add(value, value))

  @Test def testCreationOfMultiplyExpr(): Unit =
        val value = Expr.Literal(literalValue2)
        assertNotNull(Expr.Multiply(value, value))

  @Test def testEvaluateLiteralExpr(): Unit =
        assertEquals(literalValue, Expr.evaluate(Expr.Literal(literalValue)))

  @Test def testEvaluateAddExpr(): Unit =
        assertEquals(3, Expr.evaluate(Expr.Add(Expr.Literal(literalValue), Expr.Literal(literalValue2))))

  @Test def testEvaluateMultiplyExpr(): Unit =
        assertEquals(literalValue2, Expr.evaluate(Expr.Multiply(Expr.Literal(literalValue), Expr.Literal(literalValue2))))

  @Test def testShowLiteralExpr(): Unit =
        assertEquals(s"$literalValue", Expr.show(Expr.Literal(literalValue)))

  @Test def testShowAddExpr(): Unit =
        assertEquals(s"($literalValue + $literalValue2)", Expr.show(Expr.Add(Expr.Literal(literalValue), Expr.Literal(literalValue2))))

  @Test def testShowMultiplyExpr(): Unit =
        assertEquals(s"($literalValue * $literalValue2)", Expr.show(Expr.Multiply(Expr.Literal(literalValue), Expr.Literal(literalValue2))))

  @Test def testEvaluateComplexExpr(): Unit =
        assertEquals(5, Expr.evaluate(Expr.Add(Expr.Literal(literalValue), Expr.Multiply(Expr.Literal(literalValue2), Expr.Literal(literalValue2)))))