import org.junit.Test;
import trefoil2.*;

import java.util.function.Function;

import static junit.framework.TestCase.assertEquals;

public class Trefoil2Test {

    // ---------------------------------------------------------------------------------------------
    // Expression tests
    // ---------------------------------------------------------------------------------------------

    @Test
    public void testProfNil() {

        String nilBinding =
                "(define x nil)";
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env = Interpreter.interpretBinding(Binding.parseString(nilBinding), env);

        assertEquals(new Expression.BooleanLiteral(true),
                Interpreter.interpretExpression(Expression.parseString("(nil? x)"), env));
    }


    @Test
    public void testIntLit() {
        assertEquals(Expression.ofInt(3),
                Interpreter.interpretExpression(Expression.parseString("3")));
    }

    @Test
    public void testIntLitNegative() {
        assertEquals(Expression.ofInt(-10),
                Interpreter.interpretExpression(Expression.parseString("-10")));
    }

    @Test
    public void testBoolLitTrue() {
        assertEquals(new Expression.BooleanLiteral(true),
                Interpreter.interpretExpression(Expression.parseString("true")));
    }

    @Test
    public void testBoolLitFalseParsing() {
        assertEquals(Expression.ofBoolean(false), Expression.parseString("false"));
    }

    @Test
    public void testOperatorMinusParsing() {
        assertEquals(new Expression.Minus(Expression.ofInt(2), Expression.ofInt(3)), Expression.parseString("(- 2 3)"));
    }

    @Test
    public void testOperatorTimesParsing() {
        assertEquals(new Expression.Times(Expression.ofInt(4), Expression.ofInt(5)), Expression.parseString("(* 4 5)"));
    }

    @Test
    public void testOperatorEqualsParsing() {
        assertEquals(
                new Expression.Equals(Expression.ofInt(6), Expression.ofInt(7)), Expression.parseString("(= 6 7)"));
    }

    @Test
    public void testOperatorIfParsing() {
        assertEquals(new Expression.If(Expression.ofBoolean(false), Expression.ofInt(8), Expression.ofInt(9)), Expression.parseString("(if false 8 9)"));
    }

        @Test
    public void testBoolLitFalse() {
        assertEquals(new Expression.BooleanLiteral(false),
                Interpreter.interpretExpression(Expression.parseString("false")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testEmptyParen() {
        Interpreter.interpretExpression(Expression.parseString("()"));
    }

    @Test
    public void testVar() {
        assertEquals(Expression.ofInt(3),
                Interpreter.interpretExpression(Expression.parseString("x"),
                        Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(3))));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testVarNotFound() {
        Interpreter.interpretExpression(Expression.parseString("x"));
    }

    @Test
    public void testPlus() {
        assertEquals(Expression.ofInt(3),
                Interpreter.interpretExpression(Expression.parseString("(+ 1 2)")));
    }


    @Test
    public void testPlusTwo() {
        assertEquals(Expression.ofInt(20),
                Interpreter.interpretExpression(Expression.parseString("(+ (+ 2 9) (+ 4 5))")));
    }

    @Test
    public void testPlusThree() {
        assertEquals(Expression.ofInt(41),
                Interpreter.interpretExpression(Expression.parseString("(+ (+ (+ 2 2) (+ 3 3)) (+ (+ 5 8) (+ 9 9)))")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testPlusMissingOneArg() {
        Interpreter.interpretExpression(Expression.parseString("(+ 1)"));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testPlusTypeError() {
        assertEquals(Expression.ofInt(3),
                Interpreter.interpretExpression(Expression.parseString("(+ 1 true)")));
    }

    @Test
    public void testMinus() {
        assertEquals(Expression.ofInt(-1),
                Interpreter.interpretExpression(Expression.parseString("(- 1 2)")));
    }

    @Test
    public void testMinusTwo() {
        assertEquals(Expression.ofInt(0),
                Interpreter.interpretExpression(Expression.parseString("(- (- 1 2) (- 4 5))")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testMinusError() {
        assertEquals(Expression.ofInt(1920),
                Interpreter.interpretExpression(Expression.parseString("(- 4)")));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testMinusErrorTwo() {
        assertEquals(Expression.ofInt(1920),
                Interpreter.interpretExpression(Expression.parseString("(- true false)")));
    }


        @Test
    public void testTimes() {
        assertEquals(Expression.ofInt(6),
                Interpreter.interpretExpression(Expression.parseString("(* 2 3)")));
    }

    @Test
    public void testTimesTwo() {
        assertEquals(Expression.ofInt(120),
                Interpreter.interpretExpression(Expression.parseString("(* (* 5 4) (* 2 3))")));
    }

    @Test
    public void testTimesThree() {
        assertEquals(Expression.ofInt(1920),
                Interpreter.interpretExpression(Expression.parseString("(* (* (+ 2 2) (+ 2 2)) (* (* 5 4) (* 2 3)))")));
    }


    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testTimesError() {
        assertEquals(Expression.ofInt(1920),
                Interpreter.interpretExpression(Expression.parseString("(* (+ 2 2)))")));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testTimesErrorTwo() {
        assertEquals(Expression.ofInt(1920),
                Interpreter.interpretExpression(Expression.parseString("(* true false)))")));
    }

    @Test
    public void testEqualsIntTrue() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(= 3 (+ 1 2))")));
    }

    @Test
    public void testEqualsIntTrueComplex() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(= (- (* 6 6) (+ 26 10)) (- (* 4 5) (+ 10 10)))")));
    }

    @Test
    public void testEqualsIntFalse() {
        assertEquals(Expression.ofBoolean(false),
                Interpreter.interpretExpression(Expression.parseString("(= 4 (+ 1 2))")));
    }

    @Test
    public void testEqualsIntFalseComplex() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(= (- (* 6 6) (+ 26 10)) (- (* 4 5) (+ 10 10)))")));
    }


    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testEqualsIntWrongType() {
        Interpreter.interpretExpression(Expression.parseString("(= 4 true)"));
    }


    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testEqualsIntWrongTypeTwo() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(= true true)")));
    }


    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testEqualsIntParamError() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(= 5)")));
    }


    @Test
    public void testIfTrue() {
        assertEquals(Expression.ofInt(0),
                Interpreter.interpretExpression(Expression.parseString("(if true 0 1)")));
    }

    @Test
    public void testIfTrueComplex() {
        assertEquals(Expression.ofInt(41),
                Interpreter.interpretExpression(Expression.parseString("(if" +
                        " (+ (- (* 6 6) (+ 26 10)) (- (* 4 5) (+ 10 10)))" +
                        " (+ (+ (+ 2 2) (+ 3 3)) (+ (+ 5 8) (+ 9 9)))" +
                        " 6)"
                )));
    }


    @Test
    public void testIfFalse() {
        assertEquals(Expression.ofInt(1),
                Interpreter.interpretExpression(Expression.parseString("(if false 0 1)")));
    }

    @Test
    public void testIfFalseComplex() {
        assertEquals(Expression.ofInt(-10),
                Interpreter.interpretExpression(Expression.parseString("(if" +
                        " (= (* 6 6) (- (* 4 5) (+ 10 10)))" +
                        " 0" +
                        " (- 60 70))"
                )));
    }

    @Test
    public void testIfNonBool() {
        // anything not false is true
        // different from how (test ...) bindings are interpreted!!
        assertEquals(Expression.ofInt(0),
                Interpreter.interpretExpression(Expression.parseString("(if 5 0 1)")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testIfIncorrectParamNum() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(if 2 2)")));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testIfIncorrectFormatting() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(if 5 (+ 2 true) 1)")));
    }

    @Test
    public void testIfNoEval() {
        // since the condition is true, the interpreter should not even look at the else branch.
        assertEquals(Expression.ofInt(0),
                Interpreter.interpretExpression(Expression.parseString("(if true 0 x)")));
    }

    @Test
    public void testPutGetVariable() {
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putVariable("x", Expression.ofInt(42));

        assertEquals(Expression.ofInt(42), env.getVariable("x"));
    }

    @Test
    public void testPutGetVariableTwo() {
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putVariable("x", Expression.parseString("(if true 0 x)"));

        assertEquals(Expression.ofInt(0), Interpreter.interpretExpression(env.getVariable("x"), env));
    }

    @Test
    public void testPutGetVariableThree() {
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putVariable("x", Expression.parseString("(if true 0 x)"));

        assertEquals(Expression.ofInt(0), Interpreter.interpretExpression(env.getVariable("x"), env));
    }

    //////////////////////

    //(define (f params) body)
    @Test
    public void testGetPutFunction() {
        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (subtract x y) (- x y))");

        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("subtract", binding);


        Interpreter.DynamicEnvironment newEnv = Interpreter.DynamicEnvironment.empty();
        newEnv.putFunction("subtract", env.getFunction("subtract").getFunctionBinding());

        assertEquals(Expression.ofInt(3), Interpreter.interpretExpression(Expression.parseString("(subtract 5 2)"), newEnv));
    }

    @Test
    public void testPutGetFunctionTwo() {
        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("multiply", binding);


        Interpreter.DynamicEnvironment newEnv = Interpreter.DynamicEnvironment.empty();
        newEnv.putFunction("multiply", env.getFunction("multiply").getFunctionBinding());

        assertEquals(Expression.ofInt(10), Interpreter.interpretExpression(Expression.parseString("(multiply 5 2)"), newEnv));
    }


    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testPutFunctionError() {
        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");

        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("multiply", binding);

        assertEquals(Expression.ofInt(14), Interpreter.interpretExpression(Expression.parseString("(+ 2 (multiply 5 2) 2)"), env));
    }


    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testGetFunctionErrorTwo() {
        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");

        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("multiply", binding);
        env.getFunction("subtract");
    }

    /////////////////////

    @Test
    public void testGetFunctionTwo() {
        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");

        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("multiply", binding);

        assertEquals(Expression.ofInt(10), Interpreter.interpretExpression(Expression.parseString("(multiply 5 2)"), env));
    }


    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testGetFunctionError() {
        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");

        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("multiply", binding);

        assertEquals(Expression.ofInt(14), Interpreter.interpretExpression(Expression.parseString("(+ 2 (multiply 5 2) 2)"), env));
    }


    /////////////////////
    @Test
    public void testLet() {
        assertEquals(Expression.ofInt(4),
                Interpreter.interpretExpression(Expression.parseString("(let ((x 3)) (+ x 1))")));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testLetError() {
        assertEquals(Expression.ofInt(4),
                Interpreter.interpretExpression(Expression.parseString("(let ((x true)) (+ x 1))")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testLetErrorTwo() {
        assertEquals(Expression.ofInt(4),
                Interpreter.interpretExpression(Expression.parseString("(let ((x 2 2)) (+ x 1))")));
    }
    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testLetErrorThree() {
        assertEquals(Expression.ofInt(4),
                Interpreter.interpretExpression(Expression.parseString("(let ((x 2)) (+ x 1) (+ 2 2))")));
    }


    @Test
    public void testLetTwo() {
        assertEquals(Expression.ofInt(7),
                Interpreter.interpretExpression(Expression.parseString("(let ((x (+ (- 6 5) (+ 3 3)))) (* x 1))")));
    }


    @Test
    public void testLetShadow1() {
        assertEquals(Expression.ofInt(2),
                Interpreter.interpretExpression(Expression.parseString("(let ((x 1)) (let ((x 2)) x))")));
    }

    @Test
    public void testLetShadow2() {
        assertEquals(Expression.ofInt(21),
                Interpreter.interpretExpression(Expression.parseString("(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))")));
    }

    @Test
    public void testComment() {
        assertEquals(Expression.ofInt(3),
                Interpreter.interpretExpression(Expression.parseString("(+ ;asdf asdf asdf\n1 2)")));
    }

    @Test
    public void testNil() {
        assertEquals(Expression.nil(),
                Interpreter.interpretExpression(Expression.parseString("nil")));
    }


    @Test
    public void testCons() {
        assertEquals(Expression.cons(Expression.ofInt(1), Expression.ofInt(2)),
                Interpreter.interpretExpression(Expression.parseString("(cons 1 2)")));
    }

    @Test
    public void testConsTwo() {
        assertEquals(Expression.cons(Expression.ofInt(2), Expression.ofBoolean(true)),
                Interpreter.interpretExpression(Expression.parseString("(cons (+ 1 1) true)")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testConsError() {
                Interpreter.interpretExpression(Expression.parseString("(cons 1 2 3)"));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testConsErrorTwo() {
        Interpreter.interpretExpression(Expression.parseString("(cons 1 2 3)"));
    }



    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testConsErrorThree() {
                Interpreter.interpretExpression(Expression.parseString("(cons 1 hello)"));
    }


    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testConsErrorFour() {
        assertEquals(Expression.cons(Expression.ofInt(1), Expression.ofInt(2)),
                Interpreter.interpretExpression(Expression.parseString("(cons 1)")));
    }

    @Test
    public void testNilFalseQuestion() {
        assertEquals(new Expression.BooleanLiteral(false),
                Interpreter.interpretExpression(Expression.parseString("(nil? 17)")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testNilQuestionError() {
                Interpreter.interpretExpression(Expression.parseString("(nil? nil nil)"));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testNilQuestionErrorTwo() {
        Interpreter.interpretExpression(Expression.parseString("(nil? true true true)"));
    }


    @Test
    public void testNilTrueQuestion() {
        assertEquals(new Expression.BooleanLiteral(true),
                Interpreter.interpretExpression(Expression.parseString("(nil? nil)")));
    }


    @Test
    public void testConsFalseQuestion() {
        assertEquals(new Expression.BooleanLiteral(false),
                Interpreter.interpretExpression(Expression.parseString("(cons? 17)")));
    }

    @Test
    public void testConsTrueQuestion() {
        assertEquals(new Expression.BooleanLiteral(true),
                Interpreter.interpretExpression(Expression.parseString("(cons? (cons 4 5))")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testConsQuestionError() {
        Interpreter.interpretExpression(Expression.parseString("(cons? true true true)"));
    }

    // TODO: add tests for nil? and cons? here

    @Test
    public void testCar() {
        assertEquals(Expression.ofInt(1),
                Interpreter.interpretExpression(Expression.parseString("(car (cons 1 2))")));
    }

    @Test
    public void testCarTwo() {
        assertEquals(Expression.ofInt(4),
                Interpreter.interpretExpression(Expression.parseString("(car (cons (+ 2 2) true))")));
    }

    @Test
    public void testCarThree() {
        assertEquals(Expression.cons(Expression.ofInt(1), Expression.ofInt(2)),
                Interpreter.interpretExpression(Expression.parseString("(car (cons (cons 1 2) true))")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testCarError() {
                Interpreter.interpretExpression(Expression.parseString("(car (cons (cons 1 2) true) (cons (cons 1 2) true))"));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testCarErrorTwo() {
        Interpreter.interpretExpression(Expression.parseString("(car true)"));
    }

    @Test
    public void testCdrTwo() {
        assertEquals(Expression.ofInt(2),
                Interpreter.interpretExpression(Expression.parseString("(cdr (cons 1 2))")));
    }

    @Test
    public void testCdrThree() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(cdr (cons (+ 2 2) true))")));
    }

    @Test
    public void testCdrFour() {
        assertEquals(Expression.ofBoolean(true),
                Interpreter.interpretExpression(Expression.parseString("(cdr (cons (cons 1 2) true))")));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testCdrErrorTwo() {
        Interpreter.interpretExpression(Expression.parseString("(cdr (cons (cons 1 2) true) (cons (cons 1 2) true))"));
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testCdrErrorThree() {
        Interpreter.interpretExpression(Expression.parseString("(cdr true)"));
    }

    @Test
    public void testCdr() {
        assertEquals(Expression.ofInt(2),
                Interpreter.interpretExpression(Expression.parseString("(cdr (cons 1 2))")));
    }

    // ---------------------------------------------------------------------------------------------
    // Binding tests
    // ---------------------------------------------------------------------------------------------

    @Test
    public void testVarBinding() {
        assertEquals(Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(3)),
                Interpreter.interpretBinding(Binding.parseString("(define x (+ 1 2))")));
    }

    @Test
    public void testVarBindingLookup() {
        Interpreter.DynamicEnvironment env = Interpreter.interpretBinding(Binding.parseString("(define x (+ 1 2))"));

        assertEquals(Expression.ofInt(3), env.getVariable("x"));
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testBindingEmptyParen() {
        Interpreter.interpretBinding(Binding.parseString("()"));
    }

    @Test
    public void testTopLevelExpr() {
        // We don't test anything about the answer, since the interpreter just prints it to stdout,
        // and it would be too much work to try to capture this output for testing.
        // Instead, we just check that the environment is unchanged.
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(3));
        assertEquals(env, Interpreter.interpretBinding(Binding.parseString("(* 2 x)"), env));
    }
testBinding
    @Test
    public void testTestBindingPass() {
        // Who tests the tests??
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(3));
fde
        // just check that no exception is thrown here
        Interpreter.interpretBinding(Binding.parseString("(test (= 3 x))"), env);
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testTestBindingFail() {
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(3));
        Interpreter.interpretBinding(Binding.parseString("(test (= 2 x))"), env);
    }

    @Test(expected = Trefoil2.TrefoilError.AbstractSyntaxError.class)
    public void testTestBindingFailTwo() {
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(3));
        Interpreter.interpretBinding(Binding.parseString("(test (= 2 x) (= 3 x))"), env);
    }

    @Test
    public void testTestBindingSuccess() {
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(2));
        Interpreter.interpretBinding(Binding.parseString("(test (= (- 5 3) x))"), env);
    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testTestBindingBadData() {
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(3));
        Interpreter.interpretBinding(Binding.parseString("(test x)"), env);
    }

    @Test
    public void testFunctionBinding() {
        Interpreter.DynamicEnvironment env =
                Interpreter.interpretBinding(
                        Binding.parseString("(define (f x) (+ x 1))"));
        env = Interpreter.interpretBinding(
                Binding.parseString("(define y (f 2))"),
                env
        );
        assertEquals(
                Expression.ofInt(3),
                Interpreter.interpretExpression(Expression.parseString("y"), env)
        );
    }

    @Test
    public void testFunctionBindingLexicalScope() {
        Interpreter.DynamicEnvironment env =
                Interpreter.interpretBinding(
                        Binding.parseString("(define (f y) (+ x y))"),
                        Interpreter.DynamicEnvironment.singleton("x", Expression.ofInt(1))
                );
        env = Interpreter.interpretBinding(
                Binding.parseString("(define z (let ((x 2)) (f 3)))"),
                env
        );
        assertEquals(
                Expression.ofInt(4),
                Interpreter.interpretExpression(Expression.parseString("z"), env)
        );
    }

    @Test
    public void testFunctionBindingRecursive() {
        String program =
                "(define (pow base exp) " +
                        "(if (= exp 0) " +
                        "    1 " +
                        "    (* base (pow base (- exp 1)))))";
        Interpreter.DynamicEnvironment env =
                Interpreter.interpretBinding(
                        Binding.parseString(program)
                );
        env = Interpreter.interpretBinding(
                Binding.parseString("(define x (pow 2 3))"),
                env
        );
        assertEquals(
                Expression.ofInt(8),
                Interpreter.interpretExpression(Expression.parseString("x"), env)
        );
    }

    public static String countdownBinding =
            "(define (countdown n) " +
                    "(if (= n 0) " +
                    "    nil " +
                    "    (cons n (countdown (- n 1)))))";

    @Test
    public void testFunctionBindingListGenerator() {
        Interpreter.DynamicEnvironment env =
                Interpreter.interpretBinding(
                        Binding.parseString(countdownBinding)
                );
        env = Interpreter.interpretBinding(
                Binding.parseString("(define x (car (cdr (countdown 10))))"),
                env
        );
        Expression ans = Interpreter.interpretExpression(Expression.parseString("x"), env);
        assertEquals(Expression.ofInt(9), ans);
    }

    @Test
    public void testFunctionBindingListConsumer() {
        String sumBinding =
                "(define (sum l) " +
                        "(if (nil? l) " +
                        "    0 " +
                        "    (+ (car l) (sum (cdr l)))))";
        System.out.println(sumBinding);
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env = Interpreter.interpretBinding(Binding.parseString(countdownBinding), env);
        env = Interpreter.interpretBinding(Binding.parseString(sumBinding), env);
        env = Interpreter.interpretBinding(
                Binding.parseString("(define x (sum (countdown 10)))"),
                env
        );
        assertEquals(
                Expression.ofInt(55),
                Interpreter.interpretExpression(Expression.parseString("x"), env)
        );
    }

    // TODO: add a test for your new top-level "test" binding here


    //15 points for function calls

    @Test
    public void testFunctionCall() {

        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("multiply", binding);
        assertEquals(binding,  env.getFunction("multiply").getFunctionBinding());


    }

    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testFunctionCallError() {

        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("multiply", binding);
        assertEquals(binding,  env.getFunction("subtract").getFunctionBinding());

    }



    @Test(expected = Trefoil2.TrefoilError.RuntimeError.class)
    public void testFunctionCallErrorTwo() {

        Binding.FunctionBinding binding = (Binding.FunctionBinding) Binding.parseString("(define (multiply x y) (* x y))");
        Interpreter.DynamicEnvironment env = Interpreter.DynamicEnvironment.empty();
        env.putFunction("subtract", binding);
       Interpreter.interpretExpression(Expression.parseString("subtract 1 1 1"), env);


    }


}
