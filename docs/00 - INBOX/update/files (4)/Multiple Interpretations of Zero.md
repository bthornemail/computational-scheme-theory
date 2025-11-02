# Query


almost but it seems that you want to use one word terms or like the lemma of a word but if we consider lisp or prolog anything can be a term, so a list of words can be a term, but we really are saying that anyhting can be a monad or a function or an empty function which is the same thing as an empty set,  or returns a zero.  under the concept of Church numerals, then a monad is a set that returns itsself or a 1 and a Functor returns a >1 or ±1 or the number of execution or the recursion.  I'm pretty sure a functor can return a irrational number
can you onl tell me if this make more or less sense I have an Obsidian Vault that i want to make a epistemic topological graph of all keywords to related keywords in which we can deteremine the injective,  surjective, and  bijective terms by making a bipartite m-to-n graph by mapping terms to keywords or topics like this m(object keywords)-to-n(subject topics) in a  bipartite graph, but in order to normalize the terms we I want to use an idea I got from "Prolog's single data type is the term. Terms are either atoms, numbers, variables or compound terms" by considering the prolog data type as a polynomial term so that  * a polynomial function is a Horn Clause which would make * a polynomial constant a atom * a polynomial exponent a number * a polynomial indeterminate a variable * a polynomial expression is a Prolog compound term  * a polynomial root is  a Prolog fact * The non-real roots of polynomials with real coefficients that come in conjugate pairs is  a Prolog rule  We can use a an adjacency array of the  epistemic topological graph of all keywords to related keywords as a Logical Truth Table to deteremine if a Prolog Horn Clasue is Fact, Clause or  Definite clause,  by determining if the polynomial injective,  surjective, or  bijective using a polynomial as a Logic Proof of a polynomial  Equation, and using polynomial factorization to find the row with the order and the if the coefficents to determine the columns.  I think we can also use Zeroth-order logic (propositional logic), First-order logic, Second-order logic, and Higher-order logic using Monomial, Binomials, Trinomial and with Quadratic Polynomials of Multivariable Polynomials  in a Binary Quadratic Form as a field that uses topological concepts to model knowledge and belief in epistemic logic. It represents evidence as a topological space, using a zero root of a polynomial as projection point and a polynomial root of any term as a coefficient as a root as a branch point meaning that a polynomial expression describes a plane and a  polynomial function a branch cut of the corresponding polynomial function point space  Does that make any sense if we consider In the untyped lambda calculus the only primitive data type are functions, represented by lambda abstraction terms are analogous to Prolog terms  

---

“I want to build a knowledge graph in which terms are represented as algebraic objects. Inspired by Prolog’s term structure, I map keywords and topic relationships to polynomial expressions. Facts correspond to constant polynomials, compound terms to composite polynomials, and logical inference to polynomial factorization. The adjacency matrix of the graph acts as a truth table. Injective, surjective, and bijective mappings correspond to semantic richness of keyword relationships. Epistemic topology emerges by interpreting polynomial roots as knowledge points and branch cuts as logical dependencies. This builds a unified framework connecting Prolog terms, lambda calculus, and polynomial algebra.”
---

### 5.1 Multiple Interpretations of "Zero" **Church numeral 0:** ``` 0 = λf. λx. x % apply f zero times = identity ``` **Wait - this is identity, not empty!**



In my head this represents a 0D point which is a point in the Affine Plane but the pinch point or branch point in the Project Plane, my thinking of the affine plane is my own reality and the Projective Plane a shared reality wheares in the affine plane or in my reality a 0D point can be a true or false, no it can exist or not exist, it can be real or abstract, but in a shared context, 1 point doesn't mean anything in isolation, its the point of infinte possibleity bettwen two people 



---



this is 0D and 1D in prolog

Clauses with empty bodies are called facts. An example of a fact is:

human(socrates).

which is equivalent to the rule:

human(socrates) :- true.

The built-in predicate true/0 is always true.

Given the above fact, one can ask:

is socrates a human?

 ?- human(socrates).

 Yes

what things are humans?

 ?- human(X).

 X = socrates

Clauses with bodies are called rules. An example of a rule is:

mortal(X) :- human(X).

If we add that rule and ask what things are mortals?

 ?- mortal(X).

 X = socrates



---



in prolog 0D is true, but once we call it in a finite state machine or refer to it then it becomes a reference  rule of reality



i may have said that wrong but here it is in church encoding



---





All Church numerals are functions that take two parameters.  Church numerals '''0''', '''1''', '''2''', ..., are defined as follows in the [[lambda calculus]]:

: ''Starting with'' '''0''' ''not applying the function at all, proceed with'' '''1''' ''applying the function once, '''2''' applying the function twice in a row,'' '''3''' ''applying the function three times in a row, etc.'':

:<math>

\begin{array}{r|l|l}

\text{Number} & \text{Function definition} & \text{Lambda expression} \\

\hline

0 & 0\ f\ x = x & 0 = \lambda f.\lambda x.x 

\\

1 & 1\ f\ x = f\ x & 1 = \lambda f.\lambda x.f\ x 

\\

2 & 2\ f\ x = f\ (f\ x) & 2 = \lambda f.\lambda x.f\ (f\ x) 

\\

3 & 3\ f\ x = f\ (f\ (f\ x)) & 3 = \lambda f.\lambda x.f\ (f\ (f\ x))

\\

\vdots & \vdots & \vdots

\\

n & n\ f\ x = f^{\circ n}\ x & n = \lambda f.\lambda x.f^{\circ n}\ x

\end{array}

</math>

The Church numeral '''3''' is a chain of three applications of any given function in sequence, starting from some value.  The supplied function is first applied to a supplied argument and then successively to its own result.  The end result is not the number 3 (unless the supplied parameter happens to be 0 and the function is a [[successor function]]).  The function itself, and not its end result, is the Church numeral '''3'''.  The Church numeral '''3''' means simply to do something three times.  It is an [[Ostensive definition|ostensive]] demonstration of what is meant by "three times".

---

yere it is a a zero function
In mathematics, a zero (also sometimes called a root) of a real-, complex-, or generally vector-valued function 
f
{\displaystyle f}, is a member 
x
{\displaystyle x} of the domain of 
f
{\displaystyle f} such that 
f
(
x
)
{\displaystyle f(x)} vanishes at 
x
{\displaystyle x}; that is, the function 
f
{\displaystyle f} attains the value of 0 at 
x
{\displaystyle x}, or equivalently, 
x
{\displaystyle x} is a solution to the equation 
f
(
x
)
=
0
{\displaystyle f(x)=0}. A "zero" of a function is thus an input value that produces an output of 0.

---
Solution of an equation
Every equation in the unknown 
x
{\displaystyle x} may be rewritten as

f
(
x
)
=
0
{\displaystyle f(x)=0}
by regrouping all the terms in the left-hand side. It follows that the solutions of such an equation are exactly the zeros of the function 
f
{\displaystyle f}. In other words, a "zero of a function" is precisely a "solution of the equation obtained by equating the function to 0", and the study of zeros of functions is exactly the same as the study of solutions of equations.

Polynomial roots
Main article: Properties of polynomial roots
Every real polynomial of odd degree has an odd number of real roots (counting multiplicities); likewise, a real polynomial of even degree must have an even number of real roots. Consequently, real odd polynomials must have at least one real root (because the smallest odd whole number is 1), whereas even polynomials may have none. This principle can be proven by reference to the intermediate value theorem: since polynomial functions are continuous, the function value must cross zero, in the process of changing from negative to positive or vice versa (which always happens for odd functions).


----
# THIS IS IT RIGHT here
---
=== Applications ===
In [[algebraic geometry]], the first definition of an [[algebraic variety]] is through zero sets. Specifically, an [[affine algebraic set]] is the [[set intersection|intersection]] of the zero sets of several polynomials, in a [[polynomial ring]] <math>k\left[x_1,\ldots,x_n\right]</math> over a [[field (mathematics)|field]]. In this context, a zero set is sometimes called a ''zero locus''.

In [[Mathematical analysis|analysis]] and [[geometry]], any [[closed set|closed subset]] of <math>\mathbb{R}^n</math> is the zero set of a [[smooth function]] defined on all of <math>\mathbb{R}^n</math>. This extends to any [[smooth manifold]] as a corollary of [[paracompactness]]. <!-- There is obvious overlap between this and the next paragraph, but it takes someone more experienced to merge the two. -->

In [[differential geometry]], zero sets are frequently used to define [[manifold]]s. An important special case is the case that <math>f</math> is a [[smooth function]] from <math>\mathbb{R}^p</math> to <math>\mathbb{R}^n</math>. If zero is a [[regular value]] of <math>f</math>, then the zero set of <math>f</math> is a smooth manifold of dimension <math>m=p-n</math> by the [[Submersion_(mathematics)#Local_normal_form|regular value theorem]].

For example, the unit <math>m</math>-[[sphere]] in <math>\mathbb{R}^{m+1}</math> is the zero set of the real-valued function <math>f(x)=\Vert x \Vert^2-1</math>.


----

THIS LITERRALLY IS DESCRIBING WHT I AM SAYING
==Equivalence to DFA==

A [[deterministic finite automaton]] (DFA) can be seen as a special kind of NFA, in which for each state and symbol, the transition function has exactly one state. Thus, it is clear that every [[formal language]] that can be recognized by a DFA can be recognized by an NFA.

Conversely, for each NFA, there is a DFA such that it recognizes the same formal language. The DFA can be constructed using the [[powerset construction]].

This result shows that NFAs, despite their additional flexibility, are unable to recognize languages that cannot be recognized by some DFA. It is also important in practice for converting easier-to-construct NFAs into more efficiently executable DFAs. However, if the NFA has ''n'' states, the resulting DFA may have up to 2<sup>''n''</sup> states, which sometimes makes the construction impractical for large NFAs.
==NFA with ε-moves==

Nondeterministic finite automaton with ε-moves (NFA-ε) is a further generalization to NFA. In this kind of automaton, the transition function is additionally defined on the [[empty string]] ε. A transition without consuming an input symbol is called an ε-transition and is represented in state diagrams by an arrow labeled "ε". ε-transitions provide a convenient way of modeling systems whose current states are not precisely known: i.e., if we are modeling a system and it is not clear whether the current state (after processing some input string) should be q or q', then we can add an ε-transition between these two states, thus putting the automaton in both states simultaneously.

===Formal definition===
An ''NFA-ε'' is represented formally by a 5-[[tuple]], <math>(Q, \Sigma, \delta, q_0, F)</math>, consisting of
* a finite [[Set (mathematics)|set]] of [[State (computer science)|states]] <math>Q</math>
* a finite set of [[input symbol]]s called the [[Alphabet (computer science)|alphabet]] <math>\Sigma</math>
* a transition [[Function (mathematics)|function]] <math>\delta : Q \times (\Sigma \cup \{\varepsilon\}) \rightarrow \mathcal{P}(Q)</math>
* an ''initial'' (or [[Finite-state machine#Start state|''start'']]) state <math>q_0 \in Q</math>
* a set of states <math>F</math> distinguished as [[Finite-state machine#Accept .28or final.29 states|''accepting'' (or ''final'') ''states'']] <math>F \subseteq Q</math>.

Here, <math>\mathcal{P}(Q)</math> denotes the [[power set]] of <math>Q</math> and <math>\varepsilon</math> denotes empty string.

===ε-closure of a state or set of states===

For a state <math>q \in Q</math>, let <math>E(q)</math> denote the set of states that are reachable from <math>q</math> by following ε-transitions in the transition function <math>\delta</math>, i.e.,
<math>p \in E(q)</math> if there is a sequence of states <math>q_1,..., q_k</math> such that
* <math>q_1 = q</math>,
* <math>q_{i+1} \in \delta(q_i, \varepsilon)</math> for each <math>1 \le i < k</math>, and
* <math>q_k = p</math>.

<math>E(q)</math> is known as the '''epsilon closure''', (also '''ε-closure''') of <math>q</math>.

The ε-closure of a set <math>P</math> of states of an NFA is defined as the set of states reachable from any state in <math>P</math> following ε-transitions. Formally, for <math>P \subseteq Q</math>, define <math>E(P) = \bigcup\limits_{q\in P} E(q)</math>.

===Extended transition function===
Similar to NFA without ε-moves, the transition function <math>\delta</math> of an NFA-ε can be extended to strings.
Informally, <math>\delta^*(q,w)</math> denotes the set of all states the automaton may have reached when starting in state <math>q \in Q</math> and reading the string <math>w \in \Sigma^* .</math>
The function <math>\delta^*: Q \times \Sigma^* \rightarrow \mathcal{P}(Q)</math> can be defined recursively as follows.
* <math>\delta^*(q,\varepsilon) = E(q)</math>, for each state <math>q \in Q ,</math> and where <math>E</math> denotes the epsilon closure;
:''Informally:'' Reading the empty string may drive the automaton from state <math>q</math> to any state of the epsilon closure of <math>q .</math>
* <math display=inline>\delta^*(q,wa) = \bigcup_{r \in \delta^*(q,w)} E(\delta(r,a)) ,</math> for each state <math>q \in Q ,</math> each string <math>w \in \Sigma^*</math> and each symbol <math>a \in \Sigma .</math>
:''Informally:'' Reading the string <math>w</math> may drive the automaton from state <math>q</math> to any state <math>r</math> in the recursively computed set <math>\delta^*(q,w)</math>; after that, reading the symbol <math>a</math> may drive it from <math>r</math> to any state in the epsilon closure of <math>\delta(r,a) .</math> 

The automaton is said to accept a string <math>w</math> if 
:<math>\delta^*(q_0,w) \cap F \neq \emptyset ,</math> 
that is, if reading <math>w</math> may drive the automaton from its start state <math>q_0</math> to some accepting state in <math>F .</math>{{sfn|Hopcroft|Ullman|1979|p=25}}

===Example===
[[Image:NFAexample.svg|thumb|250px|The [[state diagram]] for ''M'']]
Let <math>M</math> be a NFA-ε, with a binary alphabet, that determines if the input contains an even number of 0s or an even number of 1s. Note that 0 occurrences is an even number of occurrences as well.

In formal notation, let <math display=block>M = (\{S_0, S_1, S_2, S_3, S_4\}, \{0, 1\}, \delta, S_0, \{S_1, S_3\})</math> where
the transition relation <math>\delta</math> can be defined by this [[state transition table]]:
{| class="wikitable" style="margin-left:auto;margin-right:auto; text-align:center;"
! {{diagonal split header|State|Input}}
! 0
! 1
! ε
|-
! ''S''<sub>0</sub>
| {}
| {}
| {''S''<sub>1</sub>, ''S''<sub>3</sub>}
|-
! ''S''<sub>1</sub>
| {''S''<sub>2</sub>}
| {''S''<sub>1</sub>}
| {}
|-
! ''S''<sub>2</sub>
| {''S''<sub>1</sub>}
| {''S''<sub>2</sub>}
| {}
|-
! ''S''<sub>3</sub>
| {''S''<sub>3</sub>}
| {''S''<sub>4</sub>}
| {}
|-
! ''S''<sub>4</sub>
| {''S''<sub>4</sub>}
| {''S''<sub>3</sub>}
| {}
|}
<math>M</math> can be viewed as the union of two [[deterministic finite automaton|DFA]]s: one with states <math>\{S_1, S_2\}</math> and the other with states <math>\{S_3, S_4\}</math>. 
The language of <math>M</math> can be described by the [[regular language]] given by this [[regular expression]] <math>(1^{*}01^{*}01^{*})^{*} \cup (0^{*}10^{*}10^{*})^{*}</math>.
We define <math>M</math> using ε-moves but <math>M</math> can be defined without using ε-moves.

===Equivalence to NFA===

To show NFA-ε is equivalent to NFA, first note that NFA is a special case of NFA-ε, so it remains to show for every NFA-ε, there exists an equivalent NFA.

Given an NFA with epsilon moves <math>M = (Q, \Sigma, \delta, q_0, F) ,</math>
define an NFA <math>M' = (Q, \Sigma, \delta', q_0, F') ,</math> where
:<math>F' = \begin{cases} F \cup \{ q_0 \} & \text{ if } E(q_0) \cap F \neq \{\} \\ F & \text{ otherwise } \\ \end{cases} </math>
and
:<math>\delta'(q,a) = \delta^*(q,a) </math> for each state <math>q \in Q</math> and each symbol <math>a \in \Sigma ,</math> using the extended transition function <math>\delta^*</math> defined above.

One has to distinguish the transition functions of <math>M</math> and <math>M' ,</math> viz. <math>\delta</math> and <math>\delta' ,</math> and their extensions to strings, <math>\delta</math> and <math>\delta'^* ,</math> respectively.
By construction, <math>M'</math> has no ε-transitions.

One can prove that <math>\delta'^*(q_0,w) = \delta^*(q_0,w)</math> for each string <math>w \neq \varepsilon</math>, by [[mathematical induction|induction]] on the length of <math>w .</math>

Based on this, one can show that <math>\delta'^*(q_0,w) \cap F' \neq \{\}</math> if, and only if, <math>\delta^*(q_0,w) \cap F \neq \{\},</math> for each string <math>w \in \Sigma^* :</math>
* If <math>w = \varepsilon ,</math> this follows from the definition of <math>F' .</math>
* Otherwise, let <math>w = va</math> with <math>v \in \Sigma^*</math> and <math>a \in \Sigma .</math> 
:From <math>\delta'^*(q_0,w) = \delta^*(q_0,w)</math> and <math>F \subseteq F' ,</math> we have <math display=block>\delta'^*(q_0,w) \cap F' \neq \{\} \;\Leftarrow\; \delta^*(q_0,w) \cap F \neq \{\} ;</math> we still have to show the "<math>\Rightarrow</math>" direction.
:*If <math>\delta'^*(q_0,w)</math> contains a state in <math>F' \setminus \{ q_0 \} ,</math> then <math>\delta^*(q_0,w)</math> contains the same state, which lies in <math>F</math>.
:*If <math>\delta'^*(q_0,w)</math> contains <math>q_0 ,</math> and <math>q_0 \in F ,</math> then  <math>\delta^*(q_0,w)</math> also contains a state in <math>F ,</math> viz. <math>q_0 .</math>
:*If <math>\delta'^*(q_0,w)</math> contains <math>q_0 ,</math> and <math>q_0 \not\in F ,</math> but <math>q_0\in F',</math> then there exists a state in <math>E(q_0)\cap F</math>, and the same state must be in <math display=inline>\delta^*(q_0,w) = \bigcup_{r \in \delta^*(q,v)} E(\delta(r,a)) .</math>{{sfn|Hopcroft|Ullman|1979|pp=26-27}}

Since NFA is equivalent to DFA, NFA-ε is also equivalent to DFA.
