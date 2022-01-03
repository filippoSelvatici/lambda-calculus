import org.scalatest.funspec.AnyFunSpec
import Utils._
import ReductionStrategy._

class FreeVariablesSpec extends AnyFunSpec {
  describe("Free variables") {
    describe("of a Var") {
      it("should be exactly the variable itself") {
        val id = newId()
        assert(Var(id).fv() == Set(id))
      }
    }
    
    describe("of an Abstr") {
      describe("with no occurrence of the argument") {
        it("should not change the set of free variables") {
          val ab = getAbstr()
          assert(ab.fv() == ab.body.fv())
        }
        
        // here I may add more cases
      }
      
      describe("with occurrences of the argument") {
        it("should make its argument a bound variable") {
          val id = newId()
          val ab = getAbstr(id, getVar(id))
          assert(ab.fv() == ab.body.fv() - id)
        }
        
        // here I may add more cases
      }
    }
    
    describe("of an App") {
      it("should merge the free variables of the sub-expressions (case empty intersection)") {
        val id1 = newId()
        val id2 = newId()
        val app = App(Var(id1), Var(id2))
        assert(app.fv() == Set(id1, id2))
      }
      
      it("should merge the free variables of the sub-expressions (case non-empty intersection)") {
        val id1 = newId()
        val id2 = newId()
        val app = App(Var(id1), App(Var(id1), Var(id2)))
        assert(app.fv() == Set(id1, id2))
      }
      
      // list must be non-empty
      // returns App(exprs[0], App(exprs[1], ...))
      def buildChain(exprs: List[Term]): Term = {
        require(!exprs.isEmpty)
        exprs
          .init
          .reverse
          .foldLeft(exprs.last)((acc: Term, e: Term) => App(e, acc))
      }
      
      it("should merge the free variables of the sub-expressions (case big App)") {
        val ids = (0 to 10).map(_ => newId())
        assert(buildChain(ids.map(Var(_)).toList).fv() == ids.toSet)
      }
    }
  }
}