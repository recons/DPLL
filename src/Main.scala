import scala.io.Source

/**
 * Created by sergej on 07.12.15.
 */
object Main extends App {
  val file = "test.cnf"
  val source = Source.fromFile(file)

  try {
    val cnf = DimacsParser(source)
    DPLL(cnf) match {
      case true => println("1")
      case false => println("0")
    }
  } catch {
    case (_: FileFormatException) => println("-1")
  }
  /*
  val solver = new DPLLSolver(file)
  solver.solve match {
    case true => println("1")
    case false => println("0")
  }
  */
}
