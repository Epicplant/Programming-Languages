import java.io.*;
import java.util.*;

/**2
 * Interpreter for the Trefoil v1 language.
 *
 * The interpreter's state is a stack of integers.
 *
 * The interpreter also implements a main method to accept input from the keyboard or a file.
 */
public class Trefoil {
    // TODO: probably declare one or more fields to track the state of the interpreter.
    // TODO: either initialize your fields directly or create an explicit zero-argument constructor to do so.

    private Stack<Integer> stacker;

    public Trefoil() {
        this.stacker = new Stack<>();
    }

    public static void main(String[] args) {

        Trefoil trefoil = new Trefoil();

        try {

            if (args.length == 0) {
                trefoil.interpret(new Scanner(System.in));
            } else if (args.length == 1) {
                try {
                    trefoil.interpret(new Scanner(new File(args[0])));
                } catch (FileNotFoundException e) {
                    System.out.println("No file (" + e + ") was found with which to calcualte with");   // TODO: wrong; print nice message instead
                    System.exit(1); // TODO: wrong; exit indicating error instead

                }
            } else {
                System.err.println("Expected 0 or 1 arguments but got " + args.length);
                System.exit(1);
            }

        } catch(TrefoilError e) {
            System.out.println(e);
            System.exit(1);
        }

        // print the stack
        System.out.println(trefoil);
    }

    /**
     * Interpret the program given by the scanner.
     */
    public void interpret(Scanner scanner) {
        // TODO: your interpreter here. feel free to adapt the demo code from lecture 1

            while (scanner.hasNext()) {
                if (scanner.hasNextInt()) {
                    this.stacker.push(scanner.nextInt());
                } else {

                    String token = scanner.next();

                    if (token.equals("+")) {

                        try {
                            Integer tokenOne = this.stacker.pop();
                            Integer tokenTwo = this.stacker.pop();

                            this.stacker.push(tokenOne + tokenTwo);
                        } catch (Exception error) {
                            throw new TrefoilError("Needs 2 or more values with which to add");
                        }

                    } else if (token.equals("-")) {

                        try {
                            int tokenOne = this.stacker.pop();
                            int tokenTwo = this.stacker.pop();
                            this.stacker.push(tokenTwo - tokenOne);
                        } catch (Exception error) {
                            throw new TrefoilError("Needs 2 or more values with which to subtract");
                        }

                    } else if (token.equals(".")) {

                        try {
                            System.out.println(this.stacker.pop());
                        } catch (Exception error) {
                            throw new TrefoilError("There are currently no stored values to show");
                        }

                    } else if (token.equals("*")) {

                        try {
                            this.stacker.push(this.stacker.pop() * this.stacker.pop());
                        } catch (Exception error) {
                            throw new TrefoilError("Needs 2 or more values with which to multiply");
                        }

                    } else if (!token.startsWith(";")){
                        throw new TrefoilError("This input does not meet the languages' specifications (not a token)");
                    }
                }
            }
    }

    /**
     * Convenience method to interpret the given string. Useful for unit tests.
     */
    public void interpret(String input) {
        // Don't change this method unless you know what you're doing.
        interpret(new Scanner(input));
    }

    /**
     * Pop a value off the stack and return it. Useful for unit tests.
     *
     * @throws TrefoilError if there are no elements on the stack.
     */
    public int pop() {
        // TODO: implement this
        return this.stacker.pop();
    }

    @Override
    public String toString() {
        // TODO: change this to print the stack
        //       (don't print it using Stack.toString like we did it in Lecture 1;
        //       read the spec and check the tests)

       Stack<Integer> stackStorage = new Stack<>();
       String printScan = new String();

       while(!this.stacker.isEmpty()) {
           int pop = this.stacker.pop();
           stackStorage.add(pop);

       }

       while(!stackStorage.isEmpty()) {
           int pop = stackStorage.pop();

           if(stackStorage.isEmpty()) {
               printScan += pop;
           } else {
               printScan += pop + " ";
           }

           this.stacker.push(pop);
       }

       System.out.println(printScan);

        return printScan;
    }

    /**
     * Throw this error whenever your interpreter detects a problem.
     *
     * TODO: Catch this error in main and print a nice message and exit in a way that indicates an error.
     */
    public static class TrefoilError extends RuntimeException {
        public TrefoilError(String message) {
            super(message);
        }
    }
}
