import org.junit.Test;

import java.util.Scanner;

import static junit.framework.TestCase.*;

public class TrefoilTest {


    @Test
    public void interpretOne() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1");
        assertEquals(trefoil.pop(), 1);
    }

    @Test
    public void interpretAdd() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 2 +");
        assertEquals(trefoil.pop(), 3);
    }

    @Test
    public void interpretAddSplit() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 2");
        // the interpreter should track the stack across multiple calls to interpret()
        trefoil.interpret("+");
        assertEquals(trefoil.pop(), 3);
    }

    @Test
    public void toString_() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 2 3");
        assertEquals(trefoil.toString(), "1 2 3");
    }

    // TODO: add unit tests here to cover all features in the language (don't forget to test comments!)

    @Test
    public void longInput() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 2 + 2 4 6 8 9");
        assertEquals(trefoil.toString(), "3 2 4 6 8 9");
    }

    @Test
    public void multipleOperators() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 2 + 5 8 + 3 6 - 6 7 *");
        assertEquals(trefoil.toString(), "3 13 -3 42");
    }


    @Test
    public void commentTest() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret(";commentTest");
        assertEquals(trefoil.toString(), "");
    }

    @Test
    public void commentAndAdditionalOperatorsTest() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("5 6 + 2 5 - ;commentTest 7 8 *");
        assertEquals(trefoil.toString(), "11 -3 56");
    }


    @Test(expected = Trefoil.TrefoilError.class)
    public void stackUnderflow() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 +");
    }

    @Test(expected = Trefoil.TrefoilError.class)
    public void multiplicationFail() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 *");
    }

    @Test(expected = Trefoil.TrefoilError.class)
    public void subtractionFail() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("1 -");
    }

    @Test(expected = Trefoil.TrefoilError.class)
    public void pushFail() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret(".");
    }

    @Test(expected = Trefoil.TrefoilError.class)
    public void malformedInput() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("adsgjla;ksdg");
    }

    @Test(expected = Trefoil.TrefoilError.class)
    public void misplacedOperator() {
        Trefoil trefoil = new Trefoil();
        trefoil.interpret("+ 5 5");
    }


    // TODO: add unit tests for malformed programs that the user might accidentally input
    //       you can use the @Test(expected = Trefoil.TrefoilError.class) notation above
    //       to write a test that fails if the exception is *not* thrown. Add at least
    //       one test for each operator that can signal an error, plus at least one test
    //       containing malformed input (a word that is not a token).
}