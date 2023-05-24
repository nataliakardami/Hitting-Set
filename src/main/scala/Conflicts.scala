/**
 * Find conflict sets in diagnosis problems using a theorem prover (from the gapt library), similarly to the original tp.pl.
 *
 * See https://www.logic.at/gapt/ and https://www.logic.at/gapt/downloads/gapt-user-manual.pdf
 *
 */
import gapt.expr.formula.Formula
import gapt.expr.formula.fol.FOLTerm
import gapt.expr.stringInterpolationForExpressions
import gapt.proofs.resolution.structuralCNF
import gapt.provers.escargot.Escargot

object Conflicts {

  /**
   * Map the 'normal' (~ab(component), i.e. not abnormal) to a list of terms.
   * @param l list of FOLTerm
   * @return list of formula of normal components, i.e. not abnormal
   */
  def mapNormal(l : List[FOLTerm]) : List[Formula] = {
    l.map((x : FOLTerm) => fof"-ab($x)")
  }


  /**
   * Returns conjunction formula of a list of Formulae
   * @param list list of Formula
   * @return conjunction Formula of List of Formula
   */
  def conjunction(list : List[Formula]) : Formula = {
    list.fold(fof"true")(_ & _)
  }

  /**
   * Returns disjunction formula of a list of Formulae
   * @param list list of Formula
   * @return disjunction Formula of List of Formula
   */
  def disjunction(list : List[Formula]) : Formula = {
    list.fold(fof"false")(_ | _)
  }

  /**
   * Instantiate a Formula for every element in the list of terms.
   * @param f formula to instantiate
   * @param ts list of terms to instantiate f with
   * @return list of formulas that contains f instantiated with every term in ts individually
   */
  def instantiateWithComp(f : Formula, ts : List[FOLTerm]) : List[Formula] = {
    ts.map((term : FOLTerm) => gapt.expr.formula.hol.instantiate(f, term))
  }

  /**
   * Morph a String to a FOLTerm
   * @param s String to morph
   * @return FOLTerm of the String
   */
  def stringToTerm(s : String) : FOLTerm = {
    fot"$s"
  }

  /**
   * Use a theorem prover to find the abnormal components given a system description, lists of components, observations,
   * and abnormal components respectively.
   * @param SD system description, list of formula
   * @param COMP components, list of first-order-logic terms
   * @param OBS observations, list of formula
   * @param HS abnormal components, list of first-order-logic terms
   * @return Some conflict set of terms that are the components found to be faulty,
   *         or None if there are no conflict sets found.
   */
  def tp(SD: List[Formula], COMP: List[FOLTerm], OBS: List[Formula], HS : List[FOLTerm]) : Option[Set[FOLTerm]] = {
    // 1. Subtract components defined to be abnormal.
    // 2. map 'normal' (~ab(_)) to the list of components assumed to be normal.
    val normalComponents = mapNormal(COMP.diff(HS))

    // Split system description list in to lists of quantified and standard formulas.
    val (qGates, gates) = SD.partition(gapt.expr.formula.hol.containsQuantifier(_))

    // Instantiate the quantified formulas and concatenate with the standard formulas.
    val instantiatedSD = qGates.flatMap(g => instantiateWithComp(g, COMP)) ++ gates

    // The theory is the conjunction of the instantiated system description, the assumed to be normal components and the observations.
    val Theory = conjunction(instantiatedSD ++ normalComponents ++ OBS)

    // Prove the negation of the theory, such that we can say whether there is a counterexample.
    val proof = Escargot getExpansionProof -Theory map (_.deep)

    // If proof is empty, then there is no proof found and we return None.
    if (proof.isEmpty) {
      return None
    }

    // Post-process proof to get abnormal components.
    val abnormals = structuralCNF(proof.get, propositional = true)
      // Get conclusions
      .map(_.conclusion)
      // Filter tautologies and empty sequents
      .filter(!_.isTaut)
      .filter(!_.isEmpty)
      // Get the antecedents of the sequents that are a contradiction
      .filter(_.succedent.isEmpty)
      .map(_.antecedent)

    // Extract the components inside the abnormal ab(_) definitions
    val badComponents = abnormals.map(_.toString()).map {
      case s"${x}ab(${y})${z}" => Some(y)
      case _ => None
    }.filter(_.isDefined).map(_.get)


    Some(badComponents.map(stringToTerm))
  }

  /**
   * helper function/convenience method for tp.
   * @param problem diagnosis problem as a function reference
   * @param HS list of abnormal terms. default: empty list
   * @return see tp function
   */
  def tpf(problem : () => (List[Formula], List[FOLTerm], List[Formula]), HS : List[FOLTerm] = List()) : Option[Set[FOLTerm]] = {
    val (sd, comp, obs) = problem()
    tp(sd, comp, obs, HS)
  }
}
