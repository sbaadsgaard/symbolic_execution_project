import java.util
import com.microsoft.z3.{BoolExpr, Context}
class PathConstraint(ctx: Context) {
  private var formula = ctx.mkTrue()

  def addConstraint(c: BoolExpr): Unit = {
    formula = ctx.mkAnd(formula, c)
    formula = formula.simplify().asInstanceOf[BoolExpr]
  }

  def getFormula: BoolExpr = {
    formula
  }

  def simplifyFormula(): Unit = {
    formula = formula.simplify().asInstanceOf[BoolExpr]
  }

  def ForkPathConstraint(c: BoolExpr): PathConstraint = {
    val newPC = new PathConstraint(ctx)
    newPC.addConstraint(this.formula)
    newPC.addConstraint(c)
    newPC.simplifyFormula()
    newPC
  }
}

