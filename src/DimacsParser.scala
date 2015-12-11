import scala.io.BufferedSource

/**
 * Created by sergej on 09.12.15.
 */
object DimacsParser {
  def apply(input: BufferedSource): List[List[Int]] = {
    var clauses: List[List[Int]] = Nil
    var nbClauses: Option[Int] = None
    var currentClause: List[Int] = Nil
    var nbVariables = 0

    val lines = input.mkString.split("\n").toList

    for(line <- lines) {
      val length = line.length
      if(length > 0 && line(0) != 'c' && line(0) != '%') {
        if(line.startsWith("p cnf")) {

          if(nbClauses != None)
            throw new FileFormatException("A line starting with 'p cnf' is defined twice")

          val rest = line.substring("p cnf".length, length).split(' ').filterNot(_ == "")
          try {
            val restInts = rest.map(_.toInt)
            if(restInts.length != 2)
              throw FileFormatException("")
            nbVariables = restInts(0)
            nbClauses = Some(restInts(1))
            assert(nbClauses.get > 0 && nbVariables > 0)
          } catch {
            case (_: NumberFormatException) => throw FileFormatException("")
          }

        } else { //should be a clause
          if(nbClauses == None)
            throw new FileFormatException("A line starting with 'p cnf' should occur before any clauses")

          try {
            val numbers = line.split(' ').filterNot(_ == "").map(_.toInt)

            if(!numbers.isEmpty)
              numbers.map(i => {
                if(i == 0 && currentClause != Nil) {
                  clauses ::= currentClause
                  currentClause = Nil
                } else {
                  currentClause ::= i
                }
              })
          } catch {
            case (_: NumberFormatException) => throw FileFormatException("")
          }
        }
      } //else simply ignore the line, don't need to reject the input file for that
    }

    clauses
  }
}

case class FileFormatException(msg: String) extends Exception
