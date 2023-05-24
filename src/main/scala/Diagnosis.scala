/**
 * Translation of the diagnostics problems in diagnosis.pl to Scala using the gapt library.
 *
 * See https://www.logic.at/gapt/ and https://www.logic.at/gapt/downloads/gapt-user-manual.pdf
 *
 */
import gapt.expr.formula.fol.FOLTerm
import gapt.expr.formula.Formula
import gapt.expr.stringInterpolationForExpressions

object Diagnosis {
    // ASCII Format:
    // val and_gate_ascii = hof" !x (and(x) & -ab(x) -> (in1(x) & in2(x) <-> out(x))) "
    // Generated Unicode format:
    val and_gate = fof"∀x (and(x) ∧ ¬ab(x) → in1(x) ∧ in2(x) ↔ out(x))"

    // ASCII Format:
    // val or_gate_ascii = hof" !x (or(x) & -ab(x) -> (in1(x) | in2(x) <-> out(x))) "
    // Generated Unicode format:
    val or_gate = fof"∀x (or(x) ∧ ¬ab(x) → in1(x) ∨ in2(x) ↔ out(x))"

    // ASCII Format:
    // val exor_gate = hof" !x (exor(x) & -ab(x) -> (out(x) <-> in1(x) & -in2(x) | -in1(x) & in2(x))) "
    // Generated Unicode format:
    val exor_gate = fof"∀x (exor(x) ∧ ¬ab(x) → out(x) ↔ in1(x) ∧ ¬in2(x) ∨ ¬in1(x) ∧ in2(x))"


    /**
     * Two unconnected AND gates with two inputs. It is observed that the inputs are true and the outputs are false.
     * @return tuple consisting of
     *         1. SD:   list of formulae containing the system description
     *         2. COMP: list of terms that are the components of the system
     *         3. OBS:  list of formulae that are the observations
     */
    def problem1() : (List[Formula], List[FOLTerm], List[Formula]) = {
        val SD = List(
            and_gate,
            fof"and(a1)",
            fof"and(a2)"
        )

        val COMP = List(
            fot"a1",
            fot"a2"
        )

        val OBS = List(
            fof"in1(a1)",
            fof"in2(a1)",
            fof"-out(a1)",
            fof"in1(a2)",
            fof"in2(a2)",
            fof"-out(a2)"
        )

        return (SD, COMP, OBS)
    }

    /**
     * Example of two AND gates where the output of the first gate (a1) is connected to the first input (in1)
     * of the second gate (a2). It is easy to see that the observations are inconsistent with the specification.
     * @return tuple consisting of
     *         1. SD:   list of formulae containing the system description
     *         2. COMP: list of terms that are the components of the system
     *         3. OBS:  list of formulae that are the observations
     */
    def problem2() : (List[Formula], List[FOLTerm], List[Formula]) = {
        val SD = List(
            and_gate,
            fof"and(a1)",
            fof"and(a2)",
            fof"out(a1) <-> in1(a2)"
        )

        val COMP = List(
            fot"a1",
            fot"a2"
        )

        val OBS = List(
            fof"in1(a1)",
            fof"-in2(a1)",
            fof"out(a2)"
        )

        return (SD, COMP, OBS)
    }


    /**
     * Another wiring example, now with two AND gates and an OR gate.
     * @return tuple consisting of
     *         1. SD:   list of formulae containing the system description
     *         2. COMP: list of terms that are the components of the system
     *         3. OBS:  list of formulae that are the observations
     */
    def problem3() : (List[Formula], List[FOLTerm], List[Formula]) = {
        val SD = List(
            and_gate,
            or_gate,
            fof"and(a1)",
            fof"and(a2)",
            fof"or(o1)",
            fof"out(a1) <-> in1(o1)",
            fof"out(a2) <-> in2(o1)"
        )

        val COMP = List(
            fot"a1",
            fot"a2",
            fot"o1"
        )

        val OBS = List(
            fof"in1(a1)",
            fof"in2(a1)",
            fof"in1(a2)",
            fof"in2(a2)",
            fof"-out(o1)"
        )

        return (SD, COMP, OBS)
    }

    /**
     * The following represents a (one-bit) full adder: a circuit that can be used for the addition of two bits with
     * carry-in and carry-out bits.
     *
     *   in1(fa), in2(fa): input bits
     *   carryin(fa):      carry-in bit
     *   out(fa):          output bit
     *   carryout(fa):     carry-out bit
     *
     * representation returns the sum of in1(fa) + in2(fa) + carryin(fa) as 2 * carryout(fa) + out(fa) (i.e., as 2 bits)
     * @return
     */
    def problem_fa() : (List[Formula], List[FOLTerm], List[Formula]) = {
        val SD = List(
            and_gate,
            or_gate,
            exor_gate,
            fof"and(a1)",
            fof"and(a2)",
            fof"exor(b1)",
            fof"exor(b2)",
            fof"or(r1)",
            fof"in1(fa) <-> in1(b1)",
            fof"in1(fa) <-> in1(a1)",
            fof"carryin(fa) <-> in1(a2)",
            fof"carryin(fa) <-> in2(b2)",
            fof"out(fa) <-> out(b2)",
            fof"carryout(fa) <-> out(r1)",
            fof"in2(fa) <-> in2(b1)",
            fof"in2(fa) <-> in2(a1)",
            fof"out(b1) <-> in2(a2)",
            fof"out(b1) <-> in1(b2)",
            fof"out(a2) <-> in1(r1)",
            fof"out(a1) <-> in2(r1)"
        )

        val COMP = List(
            fot"a1",
            fot"a2",
            fot"b1",
            fot"b2",
            fot"r1"
        )

        val OBS = List(
            fof"in1(fa)",
            fof"-in2(fa)",
            fof"carryin(fa)",
            fof"out(fa)",
            fof"-carryout(fa)"
        )

        return (SD, COMP, OBS)
    }
}
