
sealed trait Answer
final case class Success(value: Double) extends Answer
final case class FAIL(reason: String) extends Answer


sealed trait Expression {
  def eval: Answer
}

final case class Number(value: Double) extends Expression {
  def eval = Success(value)
}

sealed trait Operator extends Expression {

  def left: Expression
  def right: Expression
  def simpleOperation(v1: Double, v2: Double): Answer
  def eval =
  {
    val leftVal = left.eval
    val rightVal = right.eval
    (leftVal,rightVal) match {
      case (FAIL(reason),_) => FAIL(reason)
      case (_,FAIL(reason)) => FAIL(reason)
      case (Success(v1), Success(v2)) => simpleOperation(v1,v2)

    }


  }
final case class Addition(left: Expression, right: Expression) extends Operator {
  def simpleOperation(v1: Double, v2: Double) = Success(v1+ v2)

}

final case class Subtract(left: Expression, right: Expression) extends Operator {
  def simpleOperation(v1: Double, v2: Double) = Success(v1- v2)
}


final case class Divide(left: Expression, right: Expression) extends Operator {
  def simpleOperation(v1: Double, v2: Double) = {
    if(v2 == 0.0)
      FAIL("No divide by 0")
    else
      Success(v1 / v2)
  }

}
}





