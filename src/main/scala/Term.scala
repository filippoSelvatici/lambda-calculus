import Utils._
import ReductionStrategy._

sealed trait Term {
  def substitute(v: Id, e: Term): Term // substitute the free occurrences of v with e
  def fv(): Set[Id]
  def canApplyRedex(): Boolean = false
  def betaReduce()(implicit strategy: ReductionStrategy): (Term, Boolean)
  def alfaEquivalentTo(other: Term): Boolean = {
    ??? 
    // this.fvAndStructuralEq(other) && this.bindings() == other.bindings()
    // where bindings creates an AST and renames all bound variable as incremental
    // numerical values. If the bound variables are bound by the exact same Abstr, 
    // then the ASTs match perfectly
  }
  
  private def structuralEq(other: Term): Boolean =
    (this, other) match {
      case (Var(_), Var(_)) => true
      case (Abstr(_, b1), Abstr(_, b2)) => b1.structuralEq(b2)
      case (App(e1, e2), App(e3, e4)) => e1.structuralEq(e3) && e2.structuralEq(e4)
      case _ => false
    }
  private def fvAndStructuralEq(other: Term): Boolean =
    (this, other) match {
      case (Var(_), Var(_)) => this.fv() == other.fv()
      case (Abstr(_, b1), Abstr(_, b2)) => b1.structuralEq(b2) && b1.fv() == b2.fv()
      case (App(e1, e2), App(e3, e4)) => e1.structuralEq(e3) && e2.structuralEq(e4) && e1.fv() == e3.fv() && e2.fv() == e4.fv()
      case _ => false
    }
}

final case class Var(val v: Id) extends Term {
  def substitute(v: Id, e: Term): Term =
    if (this.v == v) e
    else this
  def fv(): Set[Id] = Set(v)
  def betaReduce()(implicit strategy: ReductionStrategy): (Term, Boolean) = (this, false)
}

final case class Abstr(val arg: Id, val body: Term) extends Term { // use Var instead of arg ?
  def substitute(v: Id, e: Term): Term = 
    substituteRenameWith(v, e, newId())
  def substituteRenameWith(v: Id, e: Term, z: Id): Term =  {
    if (v == arg) { this }
    else {
      val renamed = Abstr(z, body.substitute(arg, Var(z)))
      Abstr(z, renamed.body.substitute(v, e))
    }
  }
  def fv(): Set[Id] = body.fv() - arg
  def betaReduce()(implicit strategy: ReductionStrategy): (Term, Boolean) =
    strategy match {
      case NORMAL_ORDER =>  {
        val (bodyReduced, success) = body.betaReduce()
        (Abstr(arg, bodyReduced), success)
      }
      case _ => (this, false)
    }
}

final case class App(val e1: Term, val e2: Term) extends Term {
  def substitute(v: Id, e: Term): Term = App(e1.substitute(v, e), e2.substitute(v, e))
  def fv(): Set[Id] = e1.fv() ++ e2.fv()
  override def canApplyRedex(): Boolean = 
    e1 match {
      case Abstr(_, _) => true
      case _ => false
    }
  def applyRedex(): Term = {
    require(this.canApplyRedex())
    val ab = e1.asInstanceOf[Abstr]
    ab.body.substitute(ab.arg, e2)
  }
  def betaReduce()(implicit strategy: ReductionStrategy): (Term, Boolean) = {
    strategy match {
      case NORMAL_ORDER | CALL_BY_NAME => {
        if (this.canApplyRedex()) then
          return (this.applyRedex(), true)
        val (leftReduced, leftSuccess) = e1.betaReduce()
        if (leftSuccess) then
          return (App(leftReduced, e2), true)
        val (rightReduced, rightSuccess) = e2.betaReduce()
        return (App(e1, rightReduced), rightSuccess)
      }
      case CALL_BY_VALUE => {
        val (leftReduced, leftSuccess) = e1.betaReduce()
        if (leftSuccess) then
          return (App(leftReduced, e2), true)
        val (rightReduced, rightSuccess) = e2.betaReduce()
        if (rightSuccess) then
          return (App(e1, rightReduced), true)
        if (this.canApplyRedex()) then
          return (this.applyRedex(), true)
        return (this, false)
      }
    }
  }
}
