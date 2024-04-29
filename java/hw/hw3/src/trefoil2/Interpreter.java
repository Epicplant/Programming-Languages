package trefoil2;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;
import lombok.val;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Interprets expressions and bindings in the context of a dynamic environment
 * according to the semantics of Trefoil v2.
 */
public class Interpreter {

    private static DynamicEnvironment.Entry entry;
    private static DynamicEnvironment.Entry entry1;

    /**
     * Evaluates e in the given environment. Returns the resulting value.
     *
     * Throws TrefoilError.RuntimeError when the Trefoil programmer makes a mistake.
     */
    public static Expression interpretExpression(Expression e, DynamicEnvironment environment) {
        if (e instanceof Expression.IntegerLiteral) {
            return e;
        } else if (e instanceof Expression.VariableReference) {
            Expression.VariableReference var = (Expression.VariableReference) e;
            return environment.getVariable(var.getVarname());
        } else if (e instanceof Expression.Plus) {
            Expression.Plus p = (Expression.Plus) e;
            Expression v1 = interpretExpression(p.getLeft(), environment);
            Expression v2 = interpretExpression(p.getRight(), environment);
            // TODO: the following return statement is wrong because it does not correctly check
            //       for run-time type errors. fix it by checking that both children evaluated to
            //       IntegerLiterals and if not throwing TrefoilError.RuntimeError.

            if (v1 instanceof Expression.IntegerLiteral && v2 instanceof Expression.IntegerLiteral) {
                return new Expression.IntegerLiteral(
                        ((Expression.IntegerLiteral) v1).getData() +
                                ((Expression.IntegerLiteral) v2).getData()
                );
            }
                throw new Trefoil2.TrefoilError.RuntimeError("There is one or more none integer values");


        } else if (e instanceof Expression.BooleanLiteral) {

            // TODO: implement semantics for new AST nodes here, following the examples above
            // TODO: be sure to check for run-time type errors and throw TrefoilError.RuntimeError.

            if (e.toString() != "true" && e.toString() != "false") {
                throw new Trefoil2.TrefoilError.RuntimeError("This is not a boolean value");
            } else {
                return e;
            }


        } else if (e instanceof Expression.Times) {


            Expression.Times p = (Expression.Times) e;
            Expression v1 = interpretExpression(p.getLeft(), environment);
            Expression v2 = interpretExpression(p.getRight(), environment);

            if (v1 instanceof Expression.IntegerLiteral && v2 instanceof Expression.IntegerLiteral) {
                return new Expression.IntegerLiteral(
                        ((Expression.IntegerLiteral) v1).getData() *
                                ((Expression.IntegerLiteral) v2).getData()
                );
            }

            throw new Trefoil2.TrefoilError.RuntimeError("There is one or more none integer values");


        } else if (e instanceof Expression.Minus) {

            Expression.Minus p = (Expression.Minus) e;
            Expression v1 = interpretExpression(p.getLeft(), environment);
            Expression v2 = interpretExpression(p.getRight(), environment);


            if (v1 instanceof Expression.IntegerLiteral && v2 instanceof Expression.IntegerLiteral) {
                return new Expression.IntegerLiteral(
                        ((Expression.IntegerLiteral) v1).getData() -
                                ((Expression.IntegerLiteral) v2).getData()
                );
            }
            throw new Trefoil2.TrefoilError.RuntimeError("There is one or more none integer values");

        } else if (e instanceof Expression.Equals) {

            Expression.Equals p = (Expression.Equals) e;
            Expression e1 = interpretExpression(p.getLeft(), environment);
            Expression e2 = interpretExpression(p.getRight(), environment);


            if (e1 instanceof Expression.IntegerLiteral && e2 instanceof Expression.IntegerLiteral) {

                boolean test = e1.equals(e2);

                return new Expression.BooleanLiteral(test);
            }
            throw new Trefoil2.TrefoilError.RuntimeError("There is one or more none integer values");

        } else if (e instanceof Expression.Cons) {


            Expression.Cons temp = (Expression.Cons) e;

            Expression e1 = interpretExpression(temp.getValOne(), environment);
            Expression e2 = interpretExpression(temp.getValTwo(), environment);

                return Expression.cons(e1, e2);

        } else if (e instanceof Expression.Nil) {

            return new Expression.Nil();

        } else if (e instanceof Expression.NilQuestion) {

            Expression.NilQuestion temp = (Expression.NilQuestion) e;
            Expression temp2 = interpretExpression(temp.getBody(), environment);

            if (temp2 instanceof Expression.Nil) {
                return new Expression.BooleanLiteral(true);
            }
            return new Expression.BooleanLiteral(false);

        } else if (e instanceof Expression.ConsQuestion) {

            Expression.ConsQuestion temp = (Expression.ConsQuestion) e;
            Expression temp2 = interpretExpression(temp.getBody(), environment);

            if (temp2 instanceof Expression.Cons) {
                return new Expression.BooleanLiteral(true);
            }
            return new Expression.BooleanLiteral(false);

        } else if (e instanceof Expression.Car) {
            Expression.Car temp = (Expression.Car) e;
            Expression temp2 = interpretExpression(temp.getBody(), environment);


         if (temp2 instanceof Expression.Cons) {
                Expression.Cons temp3 = (Expression.Cons) temp2;
                return interpretExpression(temp3.getValOne(), environment);
         }

            throw new Trefoil2.TrefoilError.RuntimeError("Requires a Cons Value");

        } else if (e instanceof Expression.Cdr) {
            Expression.Cdr temp = (Expression.Cdr) e;
            Expression temp2 = interpretExpression(temp.getBody(), environment);

            if (temp2 instanceof Expression.Cons) {
                Expression.Cons temp3 = (Expression.Cons) temp2;
                return interpretExpression(temp3.getValTwo());
            }

           throw new Trefoil2.TrefoilError.RuntimeError("Requires a Cons Value");

        } else if (e instanceof Expression.If) {


            Expression.If p = (Expression.If) e;
            Expression e1 = interpretExpression(p.getCondition(), environment);

            if (e1 instanceof Expression.BooleanLiteral) {

               String value = ((Expression.BooleanLiteral) e1).toString();
                        if(value.equals("false")) {

                        Expression e3 = interpretExpression(p.getFalseCondition(), environment);
                           return e3;
                        }
            }


            Expression e2 = interpretExpression(p.getTrueCondition(), environment);

            return e2;

        } else if (e instanceof Expression.Let) {

            Expression.Let p = (Expression.Let) e;
            String test = p.getVarName();
            Expression test2 = p.getVarDef();

            DynamicEnvironment newEnv = environment.extendVariable(p.getVarName(), interpretExpression(p.getVarDef(), environment));
            return interpretExpression(p.getBody(), newEnv);

        } else if (e instanceof Expression.FunctionCall) {

            Expression.FunctionCall p = (Expression.FunctionCall) e;
            String funName = p.getFunName();
            List<Expression> body = p.getBody();


            DynamicEnvironment callenv = environment;
            DynamicEnvironment defenv;
            Binding.FunctionBinding binding;

            if(callenv.containsFunction(funName)) {
                DynamicEnvironment.Entry.FunctionEntry ultimateFunction = callenv.getFunction(funName);
                binding = ultimateFunction.getFunctionBinding();
                defenv = ultimateFunction.getDefiningEnvironment();
            } else {
                throw new Trefoil2.TrefoilError.RuntimeError("This function does not exist");
            }

            List<String> args = binding.getArgnames();

            if(body.size() != args.size()) {
                throw new Trefoil2.TrefoilError.RuntimeError("Incorrect number of parameters entered");
            }

            List<Expression> vals = new LinkedList<>();
            for(int i = 0; i < body.size(); i++) {
                vals.add(interpretExpression(body.get(i), callenv));
              }

            DynamicEnvironment test = environment.extendVariables(args, body);



           return interpretExpression(binding.getBody(), defenv.extendVariables(args, vals));

            /* Consider a function call expression (f args), where f stands for any
                function name, and args stands for any sequence (possibly empty) of
                expressions to be passed as arguments to f. Call the current dynamic
                environment callenv. The semantics is to first lookup f in callenv. If
                f is not mapped to anything, or maps to anything besides a function entry,
                signal an error. From the function entry mapped to by f, let (define (f params) body)
                be f's function binding and defenv its defining
                environment.  Next, evaluate each element of args in left-to-right order in
                callenv. This results in a sequence of values. Call that sequence vals.
                Then evaluate body in a new dynamic environment constructed by extending
                defenv with mappings param -> val for each param and corresponding value
                val in the lists params and values. Return the result of evaluating
                body.
                This is the hardest part of the whole assignment. Do not be surprised if you
                have to read that paragraph 5 or 10 times.*/
        } else {
            // Otherwise it's an expression AST node we don't recognize. Tell the interpreter implementor.
            throw new Trefoil2.InternalInterpreterError("\"impossible\" expression AST node " + e.getClass());
        }
    }

    /**
     * Executes the binding in the given environment, returning the new environment.
     *
     * The environment passed in as an argument is *not* mutated. Instead, it is copied
     * and any modifications are made on the copy and returned.
     *
     * Throws TrefoilError.RuntimeError when the Trefoil programmer makes a mistake.
     */

    public static DynamicEnvironment interpretBinding(Binding b, DynamicEnvironment environment) {
        if (b instanceof Binding.VariableBinding) {
            Binding.VariableBinding vb = (Binding.VariableBinding) b;
            Expression value = interpretExpression(vb.getVardef(), environment);
            System.out.println(vb.getVarname() + " = " + value);
            return environment.extendVariable(vb.getVarname(), value);
        } else if (b instanceof Binding.TopLevelExpression) {
            Binding.TopLevelExpression tle = (Binding.TopLevelExpression) b;
            System.out.println(interpretExpression(tle.getExpression(), environment));
            return environment;
        } else if (b instanceof Binding.FunctionBinding) {
            Binding.FunctionBinding fb = (Binding.FunctionBinding) b;
            DynamicEnvironment newEnvironment = environment.extendFunction(fb.getFunname(), fb);
            System.out.println(fb.getFunname() + " is defined");
            return newEnvironment;
        } else if (b instanceof Binding.TestBinding) {
            // TODO: implement the TestBinding here
            Binding.TestBinding v = (Binding.TestBinding) b;
            Expression value = interpretExpression(v.getExpression(), environment);

            if(!(value instanceof Expression.BooleanLiteral) || ((Expression.BooleanLiteral) value).toString().equals("false")) {
                throw new Trefoil2.TrefoilError.RuntimeError("No such value exists");
            }

            return environment;

        }



        // Otherwise it's a binding AST node we don't recognize. Tell the interpreter implementor.
        throw new Trefoil2.InternalInterpreterError("\"impossible\" binding AST node " + b.getClass());
    }


    // Convenience methods for interpreting in the empty environment.
    // Used for testing.
    public static Expression interpretExpression(Expression e) {
        return interpretExpression(e, new DynamicEnvironment());
    }
    public static DynamicEnvironment interpretBinding(Binding b) {
        return interpretBinding(b, DynamicEnvironment.empty());
    }


    /**
     * Represents the dynamic environment, which is a mapping from strings to "entries".
     * In the starter code, the string always represents a variable name and an entry is always a VariableEntry.
     * You will extend it to also support function names and FunctionEntries.
     */
    @Data
    public static class DynamicEnvironment {

        private Entry entry;
        private Entry entry2;

        public static abstract class Entry {
            @EqualsAndHashCode(callSuper = false)
            @Data
            public static class VariableEntry extends Entry {
                private final Expression value;
            }

            @EqualsAndHashCode(callSuper = false)
            @Data
            public static class FunctionEntry extends Entry {
                private final Binding.FunctionBinding functionBinding;

                @ToString.Exclude
                private final DynamicEnvironment definingEnvironment;
            }

            // Convenience factory methods

            public static Entry variable(Expression value) {
                return new VariableEntry(value);
            }
            public static Entry function(Binding.FunctionBinding functionBinding, DynamicEnvironment definingEnvironment) {
                return new FunctionEntry(functionBinding, definingEnvironment);
            }
        }

        // The backing map of this dynamic environment.
        private final Map<String, Entry> map;

        public DynamicEnvironment() {
            this.map = new HashMap<>();
        }

        public DynamicEnvironment(DynamicEnvironment other) {
            this.map = new HashMap<>(other.getMap());
        }

        private boolean containsVariable(String varname) {
            return map.containsKey(varname) && map.get(varname) instanceof Entry.VariableEntry;
        }

        public Expression getVariable(String varname) {
            // TODO: convert this assert to instead throw a TrefoilError.RuntimeError if the variable is not bound


            if(!containsVariable(varname)) {
                 throw new Trefoil2.TrefoilError.RuntimeError("There is no variable called " + varname + " bound");
            }



            // TODO: lookup the variable in the map and return the corresponding value
            // Hint: first, read the code for containsVariable().
            // Hint: you will likely need the value field from Entry.VariableEntry
            return ((Entry.VariableEntry) map.get(varname)).getValue();
        }

        public void putVariable(String varname, Expression value) {
            // TODO: bind the variable in the backing map
            // Hint: map.put
            // Hint: either call new Entry.VariableEntry or the factory Entry.variable
            map.put(varname, Entry.variable(value));
        }

        /**
         * Returns a *new* DynamicEnvironment extended by the binding varname -> value.
         *
         * Does not change this! Creates a copy.
         */
        public DynamicEnvironment extendVariable(String varname, Expression value) {
            DynamicEnvironment newEnv = new DynamicEnvironment(this);  // create a copy
            newEnv.putVariable(varname, value);  // mutate the copy
            return newEnv;  // return the mutated copy (this remains unchanged!)
        }

        /**
         * Returns a *new* Dynamic environment extended by the given mappings.
         *
         * Does not change this! Creates a copy.
         *
         * varnames and values must have the same length
         *
         * @param varnames variable names to bind
         * @param values values to bind the variables to
         */
        public DynamicEnvironment extendVariables(List<String> varnames, List<Expression> values) {
            DynamicEnvironment newEnv = new DynamicEnvironment(this);
            assert varnames.size() == values.size();
            for (int i = 0; i < varnames.size(); i++) {
                newEnv.putVariable(varnames.get(i), values.get(i));
            }
            return newEnv;
        }

        private boolean containsFunction(String funname) {
            return map.containsKey(funname) && map.get(funname) instanceof Entry.FunctionEntry;
        }

        public Entry.FunctionEntry getFunction(String funname) {
            // TODO: convert this assert to instead throw a TrefoilError.RuntimeError if the function is not bound
            if(!containsFunction(funname)) {
             throw new Trefoil2.TrefoilError.RuntimeError("There is no function named " + funname + "bound");
            }



            // TODO: lookup the function in the map and return the corresponding function binding
            // Hint: first, read the code for containsFunction().

            Entry.FunctionEntry temp = (Entry.FunctionEntry) map.get(funname);
            return temp;
        }

        public void putFunction(String funname, Binding.FunctionBinding functionBinding) {
            // TODO: bind the function in the backing map
            // Be careful to set up recursion correctly!
            // Hint: Pass definingEnvironment=this to the Entry.function factory, and then call map.put.
            //       That way, by the time Trefoil calls the function, everything points to
            //       the right place. Tricky!

            map.put(funname, new Entry.FunctionEntry(functionBinding, this));


        }

        public DynamicEnvironment extendFunction(String funname, Binding.FunctionBinding functionBinding) {
            DynamicEnvironment newEnv = new DynamicEnvironment(this);  // create a copy of this
            newEnv.putFunction(funname, functionBinding);  // mutate the copy
            return newEnv;  // return the copy
        }

        // Convenience factory methods

        public static DynamicEnvironment empty() {
            return new DynamicEnvironment();
        }

        public static DynamicEnvironment singleton(String varname, Expression value) {
            return empty().extendVariable(varname, value);
        }
    }
}
