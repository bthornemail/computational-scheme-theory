
Local Database = An NFA-ε is represented formally by a 5-tuple, (Q, Σ, δ, q0, F), consisting of P(Q) denotes the power set of Q and ε epsilon denotes empty string.

Local Data - Two-way deterministic finite automaton(2NFA) can be described by the following 8-tuple: M = (Q, Σ, L, R, δ, s, t, r)

User = Nondeterministic finite automaton

Public Database = Deterministic finite automaton

Signed Data = Recognized language = Given an NFA M = (Q, Σ, δ, q0, F), its recognized language is denoted by L(M)

Data Interface = Automaton = An NFA is represented formally by a 5-tuple, (Q, Σ, δ, q0, F)

Function Interface = A two-way nondeterministic finite automaton (2NFA) may have multiple transitions defined in the same configuration. Its transition function is δ : Q × (Σ ∪ {L, R}) → 2^Q × {left, right}.

Public Interface  = Two-way alternating finite automaton(2AFA) is a two-way extension of an alternating finite automaton (AFA). Its state set is Q = Q∃ ∪ Q∀ where Q∃ ∩ Q∀ = ∅.

Shared State Database = An NFA-ε is represented formally by a 5-tuple, (Q, Σ, δ, q0, F), consisting of P(Q) denotes the power set of Q and ε epsilon denotes empty string.

Peers - Two-way pushdown automaton(2PDA) is allowed to move either way on its input tape is called two-way pushdown automaton (2PDA)

AI-Agents = Sweeping automata 2DFAs process the input string by making alternating left-to-right and right-to-left sweeps, turning only at the endmarkers, each accepted by an n-state NFA, yet which is not accepted by any sweeping automata with fewer than  2^{n}

Two-way quantum finite automaton(2QFA) is a two-way extension of a quantum finite automaton (QFA).
---
== Formal description ==

Formally, a two-way deterministic finite automaton can be described by the following 8-[[tuple]]: <math>M=(Q,\Sigma,L,R,\delta,s,t,r)</math> where
* <math>Q</math> is the finite, non-empty set of ''states''
* <math>\Sigma</math> is the finite, non-empty set of input symbols
* <math>L</math> is the left endmarker
* <math>R</math> is the right endmarker
* <math>\delta: Q \times (\Sigma \cup \{L,R\}) \rightarrow Q \times \{\mathrm{left,right}\}</math>
* <math>s</math> is the start state
* <math>t</math> is the end state
* <math>r</math> is the reject state

In addition, the following two conditions must also be satisfied:
* For all <math>q \in Q</math>
:<math>\delta(q,L)=(q^\prime,\mathrm{right})</math> for some <math>q^\prime \in Q</math>
:<math>\delta(q,R)=(q^\prime,\mathrm{left})</math> for some <math>q^\prime \in Q</math>
It says that there must be some transition possible when the pointer reaches either end of the input word.
* For all symbols <math>\sigma \in \Sigma \cup \{L\}</math>{{clarify|reason='L' and 'R' are not allowed in the 2nd component of a \delta result. Probably, in the right hand side of the following 4 equations, 'L' should be fixed to 'left' and 'R' to 'right'?|date=October 2021}}
: <math>\delta(t,\sigma)=(t,R)</math>
: <math>\delta(r,\sigma)=(r,R)</math>
: <math>\delta(t,R)=(t,L)</math>
: <math>\delta(r,R)=(r,L)</math>
It says that once the automaton reaches the accept or reject state, it stays in there forever and the pointer goes to the right most symbol and cycles there infinitely.<ref>This definition has been taken from lecture notes of CS682 (Theory of Computation) by Dexter Kozen of Stanford University</ref>
