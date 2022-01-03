object Utils {
  type Id = String

  var nextId: Int = 0

  def newId(): Id = {
    val newId = nextId.toString()
    nextId += 1
    newId
  }

  def getVar(): Var = Var(newId())
  def getAbstr(): Abstr = Abstr(newId(), getVar())
  def getAbstr(e: Term): Abstr = Abstr(newId(), e)
  def getApp(): App = App(getVar(), getVar())

  // more general (but useless versions) of the above
  // TODO: make them useful by using implicits
  def getVar(id: => Id): Var = Var(id)
  def getAbstr(id: => Id, e: => Term): Abstr = Abstr(id, e)
  def getApp(e1: => Term, e2: => Term): App = App(e1, e2)
  
  def getChangingRedex(abstrArgId: Id, abstrArg: Term): App = App(Abstr(abstrArgId, Var(abstrArgId)), abstrArg)
  def getRedex(abstrArgId: Id, inner: Term, abstrArg: Term): App = App(Abstr(abstrArgId, inner), abstrArg)
}
