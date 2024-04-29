# HW5 Questions and feedback

## Questions

What difference did you discover between your Java implementation and your OCaml
implementation? Include an example Trefoil program that illustrates the difference.

One major difference that occurs is the complete reinterpretation of the dynamic environment. Rather than having
to use a bunch of functions to modify or affect a map filled with functionbindings, Ocaml
simply keeps all its values in a public list which is very easy to append. The reason this change in implementation 
is so major is because heavily affects trefoil programmers code optimization. The usage of maps in the Java version 
is incredibly inefficient in regards to adding new variables (at least the way we implemented it). The reason for this is because everytime we create a new variable or function, we copy our dynamic environment and extend it (which doesn't even take into account the extra time it takes to look up individual function/variable bindings). This can quickly cause latency and lag when many variables are created and the program tries to call 
them multiple times. Although Ocaml's list method is also pretty slow with a large number of variables/functions, it is still faster
than the method we used in our java version (because it doesn't constantly copy and save previous versions of the dynamic environment).
As a result, the change in languages can severely affect the coder as the java
version can result in the programmer being forced to minimize usage of functions/variable far more than in Ocaml.


Code example that makes for very slow running:
(
    let ((x1 2))
     (
         (let ((x2 3))
         
         ..... [Repeat about 1,000,000 times]
         
            (=
                (+ x999999 x888888)
                (- x30000 x123456)
            )  
         )    
    )
)



`LANGUAGE.md` contained a detailed explanation of structural equality to give
semantics to Trefoil's `=` operator. Explain why your implementation of `=` is
correct even though you didn't have to explicitly write a recursive "structural
equality checker" on values.

    
    The reason we don't have to write a massive recursion loop everytime we compare types in Trefoil
    is because we utilize the same structural equality definitions as Ocaml. Because of this, we can abuse
    Ocaml's ability to recurse and check for structural equality in order to write very short code in our equals
    expr (as our parent languages handles all the equality rules for us). If we changed even the smallest part of 
    our definitions of structural equality, however (so that our definition of structural equality didn't match that of Ocaml's), we would would need
    to create new recursive "structural equality checkers" as Ocaml's definition of structural equality than ours
    (meaning we can't do the same thing).    


Describe the substantial program you wrote in Trefoil. What file did you put it in?




(*Write a function take of type int * ’a list -> ’a list that takes an int called n and a list
called xs and returns the first n elements of xs in the same order as xs. You may assume that n is
non-negative and does not exceed the length of xs. Sample solution is about 5 lines.*)



Tell us the name of your Trefoil file in FEEDBACK.md so we know where to look: firsts.trefoil

 Describe your program in FEEDBACK.md: The program here was designed to mimic the functionality of the program "firsts" from hw 4 problem #6. The answer to that question is as follows:

let rec firsts xs =

  let rec looper(sx, acc) =
    match sx with
    | [] -> List.rev(acc)
    | sx::sxs ->
     let (p, q) = sx in
     looper(sxs, p :: acc)
  in looper(xs, [])

Essentially, I wanted my program to take in a list filled with smaller lists of size 2 (esentially pairs) and then create a new list
that only had the first values found in each of the smaller lists. For example, a list of [(1,2); (3,4)] would return [3; 1] (order doesn't matter for my program).

After making firsts, since my program wasn't long enough yet, I additionally made a method called "seconds" which did the same thing as firsts except it returned the second values found in each of the smaller lists.


## Feedback

Approximately how many hours did you spend on this homework?
    Answer: Approximately 25 hours

Any feedback about the lecture material corresponding to this homework?
    Answer: The lecture material was fine. It explained how structs and matches worked in trefoil pretty well so I can't
    really complain. In fact, if *I* can write matches and structs for Trefoil afterwards it is a pretty good sign that
    you explained their functionality pretty well.

Any feedback about the homework itself?
    Answer: Ok seriously. I understand that about 130 points from this homework are extra credit technically, but these
    homeworks are way too long. The fact that our course is WAY off schedule AS A RESULT of the feature creep being
    implemented into these assignments shows that the size of these assignments is a big issue. Now, I do enjoy the assignments,
    but the size of them (which distracts from other classes severely) and the fact that it is disrupting the rest of the learning schedule means something needs to change.


Anything else you want us to know?
    Answer: Nope, this about covers it.