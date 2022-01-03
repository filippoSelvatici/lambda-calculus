import org.scalatest.funspec.AnyFunSpec
import Utils._
import ReductionStrategy._

class SubstituionSpec extends AnyFunSpec {
  describe("Substition") {
    describe("of a Var") {
      it("should be susbstituted when variable matches") {
        val id = newId()
        val e = getApp()
        assert(Var(id).substitute(id, e) == e)
      }
      
      it("should not change when variable doesn't match") {
        val v = getVar()
        val e = getApp()
        assert(v.substitute(newId(), e) == v)
      }
    }
    
    describe("of an Abstr") {
      it("should not be modified by the substitution of the variable bound by this Abstr") {
        // Recall that substitution is for the free variables, 
        // but if we try to substitute a variable equal to the
        // argument, then the variable cannot be free
        val id = newId()
        val ab = getAbstr(id, getVar(id))
        val e = getApp()
        assert(ab.substitute(id, e) == ab)
      }
      
      it("should be renamed by a substitution of a variable different from the argument") {
        val id = newId()
        val ab = getAbstr(id, getVar(id))
        val id2 = newId()
        val abRenamed = getAbstr(id2, getVar(id2))
        // no match will found, but should rename the occurrences
        assert(ab.substituteRenameWith(newId(), null, id2) == abRenamed)
      }
    }
    
    describe("of an App") {
      it("should recursively apply the substitution (case changes left)") {
        val id1 = newId()
        val v = getVar()
        val app = App(Var(id1), v)
        assert(app.substitute(id1, v) == App(v, v))
      }
      
      it("should recursively apply the substitution (case changes both)") {
        val id1 = newId()
        val v = getVar()
        val app = App(Var(id1), Var(id1))
        assert(app.substitute(id1, v) == App(v, v))
      }
    }
  }
}