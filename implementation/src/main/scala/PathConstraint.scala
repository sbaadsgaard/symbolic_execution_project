import java.util
import com.microsoft.z3.{BoolExpr, Context}
class PathConstraint(ctx: Context, var formula: BoolExpr) {

  def addConstraint(c: BoolExpr): Unit = {
    formula = ctx.mkAnd(formula, c)
    formula = formula.simplify().asInstanceOf[BoolExpr]
  }

  def forkPathConstraint(c: BoolExpr): PathConstraint = {
    val newPC = new PathConstraint(ctx, this.formula)
    newPC.addConstraint(c)
    newPC
  }
}

