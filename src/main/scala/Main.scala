import Diagnosis._
import Conflicts.tpf
import gapt.expr.stringInterpolationForExpressions



object Main extends App {
  println("Running diagnostics on problem 1 with an empty list of broken components..")
  // Try adding different elements of the set of components to this list for the problems in Diagnosis
  val hs = List()
  // Try different problems and observe what the outcomes are, also try putting different combinations of components in hs
  val Some(result) = tpf(problem1, hs)
  println(result)
}
