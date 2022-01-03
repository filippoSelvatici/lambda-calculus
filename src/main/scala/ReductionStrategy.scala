object ReductionStrategy extends Enumeration {
  type ReductionStrategy = Value
  
  implicit val defaultStrategy: ReductionStrategy = NORMAL_ORDER

  val NORMAL_ORDER = Value
  val CALL_BY_NAME = Value
  val CALL_BY_VALUE = Value
}