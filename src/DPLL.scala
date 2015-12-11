/**
 * Created by sergej on 09.12.15.
 */
object DPLL {
  def apply(cnf: List[List[Int]]) : Boolean =
    DPLL(cnf, findLiterals(cnf), Map[Int, Option[Boolean]]())

  def DPLL(clauses: List[List[Int]], symbols: Set[Int], model: Map[Int, Option[Boolean]]) : Boolean = {
    if (clauses.forall(isTrue(_, model).exists(_ == true))) return true
    if (clauses.exists(isTrue(_, model).exists(_ == false))) return false

    findPureSymbol(symbols, clauses, model) match {
      case Some((p, value)) => DPLL(clauses, symbols - p, model + (p -> Some(value)))
      case None =>
        findUnitClause(clauses, model) match {
          case Some((p, value)) => DPLL(clauses, symbols - p, model + (p -> Some(value)))
          case None =>
            val p = symbols.toList(0)
            val rest = symbols - p
            DPLL(clauses, rest, model + (p -> Some(true))) || DPLL(clauses, rest, model + (p -> Some(false)))
        }
    }
  }

  def isTrue(clause: List[Int], model: Map[Int, Option[Boolean]]) : Option[Boolean] = {
    if (clause.exists(x => model.contains(Math.abs(x)) && (model(Math.abs(x)).exists(_ == true) && x > 0 || model(Math.abs(x)).exists(_ == false) && x < 0)))
      Some(true)
    else {
      if (clause.exists(x => !model.contains(Math.abs(x))))
        None
      else
        Some(false)
    }
  }

  def findLiterals(clauses: List[List[Int]]) : Set[Int] = {
    var map = Map[Int, Int]()
    for(x <- clauses; y <- x){
      map += (Math.abs(y) -> (map.getOrElse(Math.abs(y) , 0) + 1))
    }
    map.keySet
  }

  def findPureSymbol(symbols: Set[Int], clauses: List[List[Int]], model: Map[Int, Option[Boolean]]) : Option[(Int, Boolean)] = {
    def isPurePositiveLiteral(p: Int, clauses: List[List[Int]], model: Map[Int, Option[Boolean]]) =
      clauses.forall(c =>
        (c.contains(p), c.contains(-p)) match {
          case (_, false) => true
          case (_, true) => isTrue(c, model).exists(_ == true)
      })

    def isPureNegativeLiteral(p: Int, clauses: List[List[Int]], model: Map[Int, Option[Boolean]]) =
      clauses.forall(c =>
        (c.contains(p), c.contains(-p)) match {
          case (false, _) => true
          case (true, _) => isTrue(c, model).exists(_ == true)
        })

    symbols.find(isPurePositiveLiteral(_, clauses, model)) match {
      case Some(p) => Some((p, true))
      case None =>
        symbols.find(isPureNegativeLiteral(_, clauses, model)) match {
          case Some(q) => Some((q, false))
          case None => None
        }
    }
  }

  def findUnitClause(clauses: List[List[Int]], model: Map[Int, Option[Boolean]]) : Option[(Int, Boolean)] = {
    clauses.find(c => c.count(p => !model.contains(Math.abs(p))) == 1 && isTrue(c, model).isEmpty) match {
      case None => None
      case Some(c) =>
        val Some(l) = c.find(x => !model.contains(Math.abs(x)))
        l match {
          case _ : Int =>
            if (l > 0)
              Some(l, true)
            else
              Some(-l, false)
        }
    }
  }
}
