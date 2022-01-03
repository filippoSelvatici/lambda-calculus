import org.scalatest.funspec.AnyFunSpec
import Utils._
import ReductionStrategy._

class BetaReductionSpec extends AnyFunSpec {
  describe("Beta Reduction") {
    describe("of a Var") {
      it("should not be allowed to apply redex") {
        assert(!getVar().canApplyRedex())
      }
      
      it("should not modify it") {
        val v = getVar()
        assert(v.betaReduce() == (v, false))
      }
    }
    
    describe("of an Abstr") {
      it("should not be allowed to apply redex") {
        assert(!getAbstr().canApplyRedex())
      }
      
      describe("using normal order strategy") {
        implicit val strategy: ReductionStrategy = NORMAL_ORDER
        
        it("should apply the reduction to its body (case body is Var)") {
          // useless?
          val v = getVar()
          val ab = getAbstr(newId(), v)
          assert(ab.betaReduce() == (ab, false))
        }

        it("should apply the reduction to its body (case body is App)") {
          val id = newId()
          val expr = getVar()
          val redex = getChangingRedex(id, expr)
          val id2 = newId()
          assert(Abstr(id2, redex).betaReduce() == (Abstr(id2, expr), true))
        }

        it("should apply the reduction to its body (case nested Abstr)") {
          val id = newId()
          val expr = getVar()
          val redex = getChangingRedex(id, expr)
          val id2 = newId()
          val id3 = newId()
          assert(Abstr(id3, Abstr(id2, redex)).betaReduce() == (Abstr(id3, Abstr(id2, expr)), true))
        }
      }
      
      describe("using call by name strategy") {
        implicit val strategy: ReductionStrategy = CALL_BY_NAME
        
        it("should not apply the reduction to its body") {
          val id = newId()
          val expr = getVar()
          val redex = getChangingRedex(id, expr)
          val id2 = newId()
          val ab = Abstr(id2, redex)
          assert(ab.betaReduce() == (ab, false))
        }
      }
      
      describe("using call by value strategy") {
        implicit val strategy: ReductionStrategy = CALL_BY_VALUE
        
        it("should not apply the reduction to its body") {
          val id = newId()
          val expr = getVar()
          val redex = getChangingRedex(id, expr)
          val id2 = newId()
          val ab = Abstr(id2, redex)
          assert(ab.betaReduce() == (ab, false))
        }
      }
    }
    
    describe("of an App") {
      it("should be allowed to apply redex if the first expression is Abstr") {
        assert(App(getAbstr(), getVar()).canApplyRedex())
      }
      
      it("should not be allowed to apply redex if the first expression is not Abstr") {
        assert(!App(getVar(), getVar()).canApplyRedex())
      }
      
      describe("using normal order strategy") {
        implicit val strategy: ReductionStrategy = NORMAL_ORDER
        
        it("should reduce the leftmost outermost redex (case one redex)") {
          val id = newId()
          val e = getAbstr()
          val redex = getChangingRedex(id, e)
          val unimportant = getVar()
          val app = App(redex, unimportant)
          assert(app.betaReduce() == (App(e, unimportant), true))
        }
        
        it("should reduce the leftmost outermost redex (case redexes in both sub-expressions)") {
          val id = newId()
          val e = getAbstr()
          val redex = getChangingRedex(id, e)
          val id2 = newId()
          val e2 = getAbstr()
          val redex2 = getChangingRedex(id2, e2)
          val app = App(redex, redex2)
          assert(app.betaReduce() == (App(e, redex2), true))
        }
        
        it("should reduce the leftmost outermost redex (case nested redex in left sub-expression)") {
          val id = newId()
          val id2 = newId()
          val unimportant = getVar()
          val innerRedex = App(Abstr(id, Var(id)), unimportant)
          val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          val app = App(outerRedex, unimportant)
          assert(app.betaReduce() == (App(innerRedex, unimportant), true))
        }
        
        it("should reduce the leftmost outermost redex (case nested redex in right sub-expression)") {
          val id = newId()
          val id2 = newId()
          val unimportant = getVar()
          val innerRedex = App(Abstr(id, Var(id)), unimportant)
          val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          val app = App(unimportant, outerRedex)
          assert(app.betaReduce() == (App(unimportant, innerRedex), true))
        }
      }
      
      describe("using call by name strategy") {
        implicit val strategy: ReductionStrategy = CALL_BY_NAME
        
        it("should reduce the leftmost outermost redex (case one redex)") {
          val id = newId()
          val e = getAbstr()
          val redex = getChangingRedex(id, e)
          val unimportant = getVar()
          val app = App(redex, unimportant)
          assert(app.betaReduce() == (App(e, unimportant), true))
        }
        
        it("should reduce the leftmost outermost redex (case redexes in both sub-expressions)") {
          val id = newId()
          val e = getAbstr()
          val redex = getChangingRedex(id, e)
          val id2 = newId()
          val e2 = getAbstr()
          val redex2 = getChangingRedex(id2, e2)
          val app = App(redex, redex2)
          assert(app.betaReduce() == (App(e, redex2), true))
        }
        
        it("should reduce the leftmost outermost redex (case nested redex in left sub-expression)") {
          val id = newId()
          val id2 = newId()
          val unimportant = getVar()
          val innerRedex = App(Abstr(id, Var(id)), unimportant)
          val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          val app = App(outerRedex, unimportant)
          assert(app.betaReduce() == (App(innerRedex, unimportant), true))
        }
        
        it("should reduce the leftmost outermost redex (case nested redex in right sub-expression)") {
          val id = newId()
          val id2 = newId()
          val unimportant = getVar()
          val innerRedex = App(Abstr(id, Var(id)), unimportant)
          val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          val app = App(unimportant, outerRedex)
          assert(app.betaReduce() == (App(unimportant, innerRedex), true))
        }
      }
      
      describe("using call by value strategy") {
        implicit val strategy: ReductionStrategy = CALL_BY_VALUE
        
        it("should reduce the leftmost innermost redex (case one redex)") {
          val id = newId()
          val e = getAbstr()
          val redex = getChangingRedex(id, e)
          val unimportant = getVar()
          val app = App(redex, unimportant)
          assert(app.betaReduce() == (App(e, unimportant), true))
        }
        
        it("should reduce the leftmost innermost redex (case redexes in both sub-expressions)") {
          val id = newId()
          val e = getAbstr()
          val redex = getChangingRedex(id, e)
          val id2 = newId()
          val e2 = getAbstr()
          val redex2 = getChangingRedex(id2, e2)
          val app = App(redex, redex2)
          assert(app.betaReduce() == (App(e, redex2), true))
        }
        
        it("should reduce the leftmost innermost redex (case nested redex in left sub-expression)") {
          /* Wrong because in this way it reduces inside an Abstr (which is not allowed by this strategy) */
          // val id = newId()
          // val id2 = newId()
          // val unimportant = getVar()
          // val innerRedex = App(Abstr(id, Var(id)), unimportant)
          // val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          // val app = App(outerRedex, unimportant)
          // assert(app.betaReduce() == (App(App(Abstr(id2, unimportant), unimportant), unimportant), true))
          /* Correct approach */
          val id = newId()
          val id2 = newId()
          val unimportant = getVar()
          val innerRedex = App(Abstr(id, Var(id)), unimportant)
          val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          val app = App(outerRedex, unimportant)
          assert(app.betaReduce() == (App(innerRedex, unimportant), true))
        }
        
        it("should reduce the leftmost innermost redex (case nested redex in right sub-expression)") {
          /* Wrong because in this way it reduces inside an Abstr (which is not allowed by this strategy) */
          // val id = newId()
          // val id2 = newId()
          // val unimportant = getVar()
          // val innerRedex = App(Abstr(id, Var(id)), unimportant)
          // val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          // val app = App(unimportant, outerRedex)
          // // println(app)
          // assert(app.betaReduce() == (App(unimportant, App(Abstr(id2, unimportant), unimportant)), true))
          /* Correct approach */
          val id = newId()
          val id2 = newId()
          val unimportant = getVar()
          val innerRedex = App(Abstr(id, Var(id)), unimportant)
          val outerRedex = App(Abstr(id2, innerRedex), unimportant)
          val app = App(unimportant, outerRedex)
          assert(app.betaReduce() == (App(unimportant, innerRedex), true))
        }
      }
    }
  }
}
