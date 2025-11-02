the n-ball is analogous to our inner product space of the perceptron

The n-ball, denoted B^n, is the interior of a sphere S^(n-1), and sometimes also called the n-disk. (Although physicists often use the term "sphere" to mean the solid ball, mathematicians definitely do not!)

The ball of radius r centered at point {x,y,z} is implemented in the Wolfram Language as Ball[{x, y, z}, r].

BallVolume
The equation for the surface area of the n-dimensional unit hypersphere S^n gives the recurrence relation

 S_(n+2)=(2piS_n)/n. 

 ---
 The n-hypersphere (often simply called the n-sphere) is a generalization of the circle (called by geometers the 2-sphere) and usual sphere (called by geometers the 3-sphere) to dimensions n>=4. The n-sphere is therefore defined (again, to a geometer; see below) as the set of n-tuples of points (x_1, x_2, ..., x_n) such that

 x_1^2+x_2^2+...+x_n^2=R^2, 	
(1)
where R is the radius of the hypersphere.

---

The n-sphere is the set of points in (n+1)-dimensional Euclidean space that are at a fixed distance r from a central point, where the distance r may be any nonnegative real number. It is the generalization of the circle (1-sphere) and usual sphere (2-sphere) to arbitrary dimension. An n-sphere is the surface or boundary of an (n+1)-dimensional ball.

In particular:

* A 0-sphere is a pair of points {{-r,r}}, and is the boundary of a line segment (1-ball).
* A 1-sphere is a circle of radius r.
* A 2-sphere is a sphere of radius r.
* A 3-sphere is a 4-dimensional sphere.

---
I dont know enough about what a factorial is but its the recuresive boundary or the singwing of the automaton
Special forms of Gamma(1/2n) for n an integer allow the above expression to be written as

 S_n={(2^((n+1)/2)pi^((n-1)/2))/((n-2)!!)   for n odd; (2pi^(n/2))/((1/2n-1)!)   for n even, 	
(10)
where n! is a factorial and n!! is a double factorial (OEIS A072478 and A072479).

---
And just like the collapse of the planes we spoke of befor this does the same thing
Strangely enough, for the unit hypersphere, the hyper-surface area reaches a maximum and then decreases towards 0 as n increases. The point of maximal hyper-surface area satisfies

 (dS_n)/(dn)=(pi^(n/2)[lnpi-psi_0(1/2n)])/(Gamma(1/2n))=0, 	
(11)
where psi_0(x)=Psi(x) is the digamma function. This cannot be solved analytically for n, but the numerical solution is n=7.25695... (OEIS A074457; Wells 1986, p. 67). As a result, the seven-dimensional unit hypersphere has maximum hyper-surface area (Le Lionnais 1983; Wells 1986, p. 60).

In four dimensions, the generalization of spherical coordinates is given by

x_1	=	Rsinpsisinphicostheta	
(12)
x_2	=	Rsinpsisinphisintheta	
(13)
x_3	=	Rsinpsicosphi	
(14)
x_4	=	Rcospsi.	
(15)
The equation for the 3-sphere S^3 is therefore

 x_1^2+x_2^2+x_3^2+x_4^2=R^2, 	
(16)
and the line element is

 ds^2=R^2[dpsi^2+sin^2psi(dphi^2+sin^2phidtheta^2)]. 	
(17)
By defining r=Rsinpsi, the line element can be rewritten

 ds^2=(dr^2)/((1-(r^2)/(R^2)))+r^2(dphi^2+sin^2phidtheta^2). 	
(18)
The hyper-surface area is therefore given by

S_3	=	int_0^piRdpsiint_0^piRsinpsidphiint_0^(2pi)Rsinpsisinphidtheta	
(19)
	=	2pi^2R^3.

---

And this is how we extract the semantic meaning of the perceptron
Sphere tetrahedron picking is the selection of quadruples of points corresponding to vertices of a tetrahedron with vertices on the surface of a sphere. n random tetrahedra can be picked on a unit sphere in the Wolfram Language using the function RandomPoint[Sphere[], {n, 4}].

Pick four points on a sphere. What is the probability that the tetrahedron having these points as polyhedron vertices contains the center of the sphere? In the one-dimensional case, the probability that a second point is on the opposite side of 1/2 is 1/2. In the two-dimensional case, pick t

this is how how we get a basis for incidence graph
The Riemann sphere, also called the extended complex plane, is a one-dimensional complex manifold C^* (C-star) which is the one-point compactification of the complex numbers C^*=C union {infty^~}, together with two charts. (Here infty^~ denotes complex infinity.) The notation C^^ is also used (Krantz 1999, p. 82; Lorentzen, and Waadeland 2008, p. 3).

For all points in the complex plane, the chart is the identity map from the sphere (with infinity removed) to the complex plane. For the point at infinity, the chart neighborhood is the sphere (with the origin removed), and the chart is given by sending infinity to 0 and all other points z to 1/z.


---

# The Closure of the Perceptron

In [[mathematics]], a '''profinite group''' is a [[topological group]] that is in a certain sense assembled from a system of [[finite group]]s.

The idea of using a profinite group is to provide a "uniform", or "synoptic", view of an entire system of finite groups. Properties of the profinite group are generally speaking uniform properties of the system. For example, the profinite group is finitely generated (as a topological group) if and only if there exists <math>d\in\N</math> such that every group in the system can be [[generating set of a group|generated]] by <math>d</math> elements.<ref>{{cite arXiv |last=Segal |first=Dan |date=2007-03-29 |title=Some aspects of profinite group theory |eprint=math/0703885 }}</ref> Many theorems about finite groups can be readily generalised to profinite groups; examples are  [[Lagrange's theorem (group theory)|Lagrange's theorem]] and the [[Sylow theorems]].<ref>{{Cite book |last=Wilson |first=John Stuart |title=Profinite groups |date=1998 |publisher=Clarendon Press |isbn=9780198500827 |location=Oxford |oclc=40658188}}</ref>

To construct a profinite group one needs a system of finite groups and [[group homomorphism]]s between them. Without loss of generality, these homomorphisms can be assumed to be [[Surjective function|surjective]], in which case the finite groups will appear as [[quotient group]]s of the resulting profinite group; in a sense, these quotients approximate the profinite group.

Important examples of profinite groups are the [[abelian group|additive group]]s of [[P-adic number|<math>p</math>-adic integers]] and the [[Galois group]]s of infinite-degree [[field extension]]s.

Every profinite group is [[Compact space|compact]] and [[Totally disconnected space|totally disconnected]]. A non-compact generalization of the concept is that of [[locally profinite group]]s. Even more general are the [[totally disconnected group]]s.

==Definition==

Profinite groups can be defined in either of two equivalent ways.

===First definition (constructive)===

A profinite group is a topological group that is [[isomorphism|isomorphic]] to the [[inverse limit]] of an [[inverse system]] of [[discrete space|discrete]] finite groups.<ref>{{Cite web|url=http://websites.math.leidenuniv.nl/algebra/Lenstra-Profinite.pdf|title=Profinite Groups|last=Lenstra|first=Hendrik|website=Leiden University}}</ref> In this context, an inverse system consists of a [[directed set]]  <math>(I, \leq),</math> an [[indexed family]] of finite groups <math>\{G_i: i \in I\},</math> each having the [[discrete topology]], and a family of [[Group homomorphism|homomorphisms]] <math>\{f^j_i : G_j \to G_i \mid i, j \in I, i \leq j\}</math> such that <math>f_i^i</math> is the [[identity map]] on <math>G_i</math> and the collection satisfies the composition property <math>f^j_i \circ f^k_j = f^k_i</math> whenever <math>i\leq j\leq k.</math> The inverse limit is the set:
<math display=block>\varprojlim G_i = \left\{(g_i)_{i \in I} \in {\textstyle\prod\limits_{i \in I}} G_i : f^j_i (g_j) = g_i \text{ for all } i\leq j\right\}</math>
equipped with the [[Subspace topology|relative]] [[product topology]].

One can also define the inverse limit in terms of a [[universal property]]. In [[category theory|categorical]] terms, this is a special case of a [[filtered category|cofiltered limit]] construction.

===Second definition (axiomatic)===

A profinite group is a [[Hausdorff space|Hausdorff]], [[compact group|compact]] and [[totally disconnected]] topological group:<ref name=":0">{{Cite web|url=https://www.math.ucdavis.edu/~osserman/classes/250C/notes/profinite.pdf|title=Inverse limits and profinite groups|last=Osserman|first=Brian|website=University of California, Davis|archive-url=https://web.archive.org/web/20181226233013/https://www.math.ucdavis.edu/~osserman/classes/250C/notes/profinite.pdf|archive-date=2018-12-26}}</ref> that is, a topological group that is also a [[Stone space]].

===Profinite completion===

Given an arbitrary group <math>G</math>, there is a related profinite group <math>\widehat{G},</math> the {{em|{{visible anchor|profinite completion}}}} of <math>G</math>.<ref name=":0" /> It is defined as the inverse limit of the groups <math>G/N</math>, where <math>N</math> runs through the [[normal subgroup]]s in <math>G</math> of finite [[Index of a subgroup|index]] (these normal subgroups are [[partial order|partially ordered]] by inclusion, which translates into an inverse system of natural homomorphisms between the quotients).

There is a natural homomorphism <math>\eta : G \to \widehat{G}</math>, and the image of <math>G</math> under this homomorphism is [[dense set|dense]] in <math>\widehat{G}</math>. The homomorphism <math>\eta</math> is injective if and only if the group <math>G</math> is [[residually finite group|residually finite]] (i.e.,
<math>\bigcap N = 1</math>, where the intersection runs through all normal subgroups <math>N</math> of finite index).

The homomorphism <math>\eta</math> is characterized by the following [[universal property]]: given any profinite group <math>H</math> and any continuous group homomorphism <math>f : G \rightarrow H</math> where <math>G</math> is given the smallest topology compatible with group operations in which its normal subgroups of finite index are open, there exists a unique [[continuous function (topology)|continuous]] group homomorphism <math>g : \widehat{G} \rightarrow H</math> with <math>f = g \eta</math>.

===Equivalence===

Any group constructed by the first definition satisfies the axioms in the second definition.

Conversely, any group <math>G</math> satisfying the axioms in the second definition can be constructed as an inverse limit according to the first definition using the inverse limit <math>\varprojlim G/N</math> where <math>N</math> ranges through the open [[normal subgroup]]s of <math>G</math> ordered by (reverse) inclusion. If <math>G</math> is topologically finitely generated then it is in addition equal to its own profinite completion.<ref>{{cite journal | last1=Nikolov| first1=Nikolay | last2=Segal| first2=Dan | title=On finitely generated profinite groups. I: Strong completeness and uniform bounds. II: Products in quasisimple groups | zbl=1126.20018 | journal=Ann. Math. |series=Second series | volume=165 | issue=1 | pages=171–238, 239–273 |date=2007 | doi=10.4007/annals.2007.165.171 | s2cid=15670650 | arxiv=math/0604399 }}</ref>

===Surjective systems{{anchor|Surjective system}}===

In practice, the inverse system of finite groups is almost always {{em|{{visible anchor|surjective inverse system|text=surjective}}}}, meaning that all its maps are surjective. Without loss of generality, it suffices to consider only surjective systems since given any inverse system, it is possible to first construct its profinite group <math>G,</math> and then {{em|reconstruct}} it as its own profinite completion.

==Examples==

* Finite groups are profinite, if given the [[discrete topology]].
* The group of [[p-adic number|<math>p</math>-adic integers]] <math>\Z_p</math> under addition is profinite (in fact [[#Procyclic group|procyclic]]). It is the inverse limit of the finite groups <math>\Z/p^n\Z</math> where <math>n</math> ranges over all [[natural number]]s and the natural maps <math>\Z/p^n\Z \to \Z/p^m\Z</math> for <math>n \ge m.</math> The topology on this profinite group is the same as the topology arising from the <math>p</math>-adic valuation on <math>\Z_p.</math>
* The group of [[profinite integer]]s <math>\widehat{\Z}</math> is the profinite completion of <math>\Z.</math> In detail, it is the inverse limit of the finite groups <math>\Z/n\Z</math> where <math>n = 1,2,3,\dots</math> with the modulo maps <math>\Z/n\Z \to \Z/m\Z</math> for <math>m\,|\,n.</math> This group is the product of all the groups <math>\Z_p,</math> and it is the [[absolute Galois group]] of any [[finite field]].
* The [[Galois theory]] of [[field extension]]s of infinite degree gives rise naturally to Galois groups that are profinite. Specifically, if <math>L / K</math> is a [[Galois extension]], consider the group <math>G = \operatorname{Gal}(L / K)</math> consisting of all [[field automorphism]]s of <math>L</math> that keep all elements of <math>K</math> fixed. This group is the inverse limit of the finite groups <math>\operatorname{Gal}(F / K),</math> where <math>F</math> ranges over all intermediate fields such that <math>F / K</math> is a {{em|finite}} Galois extension. For the limit process, the restriction homomorphisms <math>\operatorname{Gal}(F_1 / K) \to \operatorname{Gal}(F_2 / K)</math> are used, where <math>F_2 \subseteq F_1.</math> The topology obtained on <math>\operatorname{Gal}(L / K)</math> is known as the ''[[Krull topology]]'' after [[Wolfgang Krull]]. {{harvtxt|Waterhouse|1974}} showed that {{em|every}} profinite group is isomorphic to one arising from the Galois theory of {{em|some}} field <math>K,</math> but one cannot (yet) control which field <math>K</math> will be in this case.  In fact, for many fields <math>K</math> one does not know in general precisely which [[finite group]]s occur as Galois groups over <math>K.</math> This is the [[inverse Galois problem]] for a field&nbsp;<math>K.</math> (For some fields <math>K</math> the inverse Galois problem is settled, such as the field of [[rational function]]s in one variable over the complex numbers.)  Not every profinite group occurs as an [[absolute Galois group]] of a field.<ref name=FJ497>Fried & Jarden (2008) p.&nbsp;497</ref>
* The [[Étale fundamental group|étale fundamental groups considered in algebraic geometry]] are also profinite groups, roughly speaking because the algebra can only 'see' finite coverings of an [[algebraic variety]]. The [[fundamental group]]s of [[algebraic topology]], however, are in general not profinite: for any prescribed group, there is a 2-dimensional [[CW complex]] whose fundamental group equals it.
* The automorphism group of a [[locally finite rooted tree]] is profinite.

==Properties and facts==

* Every [[direct product of groups|product]] of (arbitrarily many) profinite groups is profinite; the topology arising from the profiniteness agrees with the [[product topology]]. The inverse limit of an inverse system of profinite groups with continuous transition maps is profinite and the inverse limit functor is [[Exact functor|exact]] on the category of profinite groups. Further, being profinite is an extension property.
* Every [[closed set|closed]] subgroup of a profinite group is itself profinite; the topology arising from the profiniteness agrees with the [[subspace (topology)|subspace topology]]. If <math>N</math> is a closed normal subgroup of a profinite group <math>G,</math> then the [[factor group]] <math>G / N</math> is profinite; the topology arising from the profiniteness agrees with the [[quotient topology]].
* Since every profinite group <math>G</math> is compact Hausdorff, there exists a [[Haar measure]] on <math>G,</math> which allows us to measure the "size" of subsets of <math>G,</math> compute certain [[probabilities]], and [[integral|integrate]] functions on <math>G.</math>
* A subgroup of a profinite group is open if and only if it is closed and has finite [[Index of a subgroup|index]].
* According to a theorem of [[Nikolay Nikolov (mathematician)|Nikolay Nikolov]] and [[Dan Segal]], in any topologically finitely generated profinite group (that is, a profinite group that has a [[dense set|dense]] [[finitely generated subgroup]]) the subgroups of finite index are open. This generalizes an earlier analogous result of [[Jean-Pierre Serre]] for topologically finitely generated [[pro-p group|pro-<math>p</math> group]]s. The proof uses the [[classification of finite simple groups]].
* As an easy corollary of the Nikolov–Segal result above, {{em|any}} surjective discrete group homomorphism <math>\varphi : G \to H</math> between profinite groups <math>G</math> and <math>H</math> is continuous as long as <math>G</math> is topologically finitely generated. Indeed, any open subgroup of <math>H</math> is of finite index, so its preimage in <math>G</math> is also of finite index, and hence it must be open.
* Suppose <math>G</math> and <math>H</math> are topologically finitely generated profinite groups that are isomorphic as discrete groups by an isomorphism <math>\iota.</math> Then <math>\iota</math> is bijective and continuous by the above result. Furthermore, <math>\iota^{-1}</math> is also continuous, so <math>\iota</math> is a homeomorphism. Therefore the topology on a topologically finitely generated profinite group is uniquely determined by its {{em|algebraic}} structure.

==Ind-finite groups==

There is a notion of {{em|{{visible anchor||text=ind-finite group}}}}, which is the conceptual [[dual (category theory)|dual]] to profinite groups; i.e. a group <math>G</math> is ind-finite if it is the [[direct limit]] of an [[Direct limit#Formal definition|inductive system]] of finite groups. (In particular, it is an [[ind-group]].) The usual terminology is different: a group <math>G</math> is called [[locally finite group|locally finite]] if every finitely generated [[subgroup]] is finite. This is equivalent, in fact, to being 'ind-finite'.

By applying [[Pontryagin duality]], one can see that [[abelian group|abelian]] profinite groups are in duality with locally finite discrete abelian groups. The latter are just the abelian [[torsion group]]s.

==Projective profinite groups==

A profinite group is {{em|{{visible anchor|projective profinite groups|projective|text=projective}}}} if it has the [[lifting property]] for every extension.  This is equivalent to saying that <math>G</math> is projective if for every surjective morphism from a profinite <math>H \to G</math> there is a [[Section (category theory)|section]] <math>G \to H.</math><ref name=S9758>Serre (1997) p.&nbsp;58</ref><ref name=FJ207>Fried & Jarden (2008) p.&nbsp;207</ref>

Projectivity for a profinite group <math>G</math> is equivalent to either of the two properties:<ref name=S9758/>
* the [[cohomological dimension]] <math>\operatorname{cd}(G) \leq 1;</math>
* for every prime <math>p</math> the Sylow <math>p</math>-subgroups of <math>G</math> are free pro-<math>p</math>-groups.

Every projective profinite group can be realized as an [[absolute Galois group]] of a [[pseudo algebraically closed field]]. This result is due to [[Alexander Lubotzky]] and [[Lou van den Dries]].<ref>Fried & Jarden (2008) pp.&nbsp;208,545</ref>

==Procyclic group==

A profinite group <math>G</math> is {{em|{{visible anchor|procyclic group|procyclic|text=procyclic}}}} if it is topologically generated by a single element <math>\sigma;</math> that is, if <math>G = \overline{\langle \sigma \rangle},</math> the closure of the subgroup <math>\langle \sigma \rangle = \left\{\sigma^n: n \in \Z\right\}.</math><ref>{{Cite book|last=Neukirch|first=Jürgen|url=http://link.springer.com/10.1007/978-3-662-03983-0|title=Algebraic Number Theory|date=1999|publisher=Springer Berlin Heidelberg|isbn=978-3-642-08473-7|series=Grundlehren der mathematischen Wissenschaften|volume=322|location=Berlin, Heidelberg|doi=10.1007/978-3-662-03983-0}}</ref>

A topological group <math>G</math> is procyclic if and only if <math>G \cong {\textstyle\prod\limits_{p\in S}} G_p</math> where <math>p</math> ranges over some set of [[prime number]]s <math>S</math> and <math>G_p</math> is isomorphic to either <math>\Z_p</math> or <math>\Z/p^n \Z, n \in \N.</math><ref>{{Cite web|url=https://mathoverflow.net/questions/247731/decomposition-of-procyclic-groups|title=MO. decomposition of procyclic groups|website=MathOverflow}}</ref>


---

In [[mathematics]], a '''profinite integer''' is an element of the [[ring (mathematics)|ring]] (sometimes pronounced as zee-hat or zed-hat)
:<math>\widehat{\mathbb{Z}} = \varprojlim \mathbb{Z}/n\mathbb{Z},</math>
where the [[inverse limit]] of the [[quotient ring]]s <math>\mathbb{Z}/n\mathbb{Z}</math> runs through all [[natural number]]s <math>n</math>, [[Partial order|partially ordered]] by [[divisibility]]. By definition, this ring is the [[profinite completion]] of the [[integer]]s <math>\mathbb{Z}</math>. By the [[Chinese remainder theorem]], <math>\widehat{\mathbb{Z}}</math> can also be understood as the [[direct product of rings]]
:<math>\widehat{\mathbb{Z}} = \prod_p \mathbb{Z}_p,</math>
where the index <math>p</math> runs over all [[prime number]]s, and <math>\mathbb{Z}_p</math> is the ring of [[p-adic integer|''p''-adic integers]]. This group is important because of its relation to [[Galois theory]], [[étale homotopy theory]], and the ring of [[Ring of adeles|adeles]]. In addition, it provides a basic tractable example of a [[profinite group]].

== Construction ==

The profinite integers <math>\widehat{\Z}</math> can be constructed as the set of sequences <math>\upsilon</math> of residues represented as
<math display="block">\upsilon = (\upsilon_1 \bmod 1, ~ \upsilon_2 \bmod 2, ~ \upsilon_3 \bmod 3, ~ \ldots)</math>
such that <math>m \ |\ n \implies \upsilon_m \equiv \upsilon_n \bmod m</math>.

Pointwise addition and multiplication make it a commutative ring.

The ring of [[integer|integers]] embeds into the ring of profinite integers by the canonical injection:
<math display="block">\eta: \mathbb{Z} \hookrightarrow \widehat{\mathbb{Z}}</math> where <math> n \mapsto (n \bmod 1, n \bmod 2, \dots).</math>
It is canonical since it satisfies the [[Profinite group#Profinite completion|universal property of profinite groups]] that, given any profinite group <math>H</math> and any group homomorphism <math>f : \Z \rightarrow H</math>, there exists a unique [[Continuous function (topology)|continuous]] group homomorphism <math>g : \widehat{\Z} \rightarrow H</math> with <math>f = g \eta</math>.

=== Using Factorial number system ===

Every integer <math>n \ge 0</math> has a unique representation in the [[factorial number system]] as
<math display="block">n = \sum_{i=1}^\infty c_i i! \qquad \text{with } c_i \in \Z</math>
where <math>0 \le c_i \le i</math> for every <math>i</math>, and only finitely many of <math>c_1,c_2,c_3,\ldots</math> are nonzero.

Its factorial number representation can be written as <math>(\cdots c_3 c_2 c_1)_!</math>.

In the same way, a profinite integer can be uniquely represented in the factorial number system as an infinite string <math>(\cdots c_3 c_2 c_1)_!</math>, where each <math>c_i</math> is an integer satisfying <math>0 \le c_i \le i</math>.<ref name="lenstra">{{cite web |last1=Lenstra |first1=Hendrik |title=Profinite number theory |url=https://www.maa.org/sites/default/files/images/mathfest/2016/pntt.pdf |website=Mathematical Association of America |access-date=11 August 2022}}</ref>

The digits <math>c_1, c_2, c_3, \ldots, c_{k-1}</math> determine the value of the profinite integer mod <math>k!</math>. More specifically, there is a ring homomorphism <math>\widehat{\Z}\to \Z / k! \, \Z</math> sending
<math display="block">(\cdots c_3 c_2 c_1)_! \mapsto \sum_{i=1}^{k-1} c_i i! \mod k!</math>
The difference of a profinite integer from an integer is that the "finitely many nonzero digits" condition is dropped, allowing for its factorial number representation to have infinitely many nonzero digits.

=== Using the Chinese Remainder theorem ===

Another way to understand the construction of the profinite integers is by using the [[Chinese remainder theorem]]. Recall that for an integer <math>n</math> with [[prime factorization]]
<math display="block">n = p_1^{a_1}\cdots p_k^{a_k}</math>
of non-repeating primes, there is a [[ring isomorphism]]
<math display="block">\mathbb{Z}/n \cong \mathbb{Z}/p_1^{a_1}\times \cdots \times \mathbb{Z}/p_k^{a_k}</math>
from the theorem. Moreover, any [[surjection]]
<math display="block">\mathbb{Z}/n \to \mathbb{Z}/m</math>
will just be a map on the underlying decompositions where there are induced surjections
<math display="block">\mathbb{Z}/p_i^{a_i} \to \mathbb{Z}/p_i^{b_i}</math>
since we must have <math>a_i \geq b_i</math>. It should be much clearer that under the inverse limit definition of the profinite integers, we have the isomorphism
<math display="block">\widehat{\mathbb{Z}} \cong \prod_p \mathbb{Z}_p</math>
with the direct product of ''p''-adic integers.

Explicitly, the isomorphism is <math>\phi: \prod_p \mathbb{Z}_p \to \widehat\Z</math> by
<math display="block">\phi((n_2, n_3, n_5, \cdots))(k) = \prod_{q} n_q \mod k</math>
where <math>q</math> ranges over all prime-power factors <math>p_i^{d_i}</math> of <math>k</math>, that is, <math>k = \prod_{i=1}^l p_i^{d_i}</math> for some different prime numbers <math>p_1, ..., p_l</math>.


---
=== Class field theory and the profinite integers ===
[[Class field theory]] is a branch of [[algebraic number theory]] studying the abelian field extensions of a field. Given the [[global field]] <math>\mathbb{Q}</math>, the [[abelianization]] of its absolute Galois group
<math display="block">\text{Gal}(\overline{\mathbb{Q}}/\mathbb{Q})^{ab}</math>
is intimately related to the associated ring of adeles <math>\mathbb{A}_\mathbb{Q}</math> and the group of profinite integers. In particular, there is a map, called the [[Artin map]]<ref>{{Cite web|title=Class field theory - lccs|url=http://www.math.columbia.edu/~chaoli/docs/ClassFieldTheory.html#sec13|access-date=2020-09-25|website=www.math.columbia.edu}}</ref>
<math display="block">\Psi_\mathbb{Q}:\mathbb{A}_\mathbb{Q}^\times / \mathbb{Q}^\times \to
\text{Gal}(\overline{\mathbb{Q}}/\mathbb{Q})^{ab}</math>
which is an isomorphism. This quotient can be determined explicitly as

<math display="block">\begin{align}
\mathbb{A}_\mathbb{Q}^\times/\mathbb{Q}^\times &\cong (\mathbb{R}\times \hat{\mathbb{Z}})/\mathbb{Z} \\
&= \underset{\leftarrow}{\lim} \mathbb({\mathbb{R}}/m\mathbb{Z}) \\
&= \underset{x \mapsto x^m}{\lim} S^1 \\
&= \hat{\mathbb{Z}}
\end{align}</math>

giving the desired relation. There is an analogous statement for [[local class field theory]] since every finite abelian extension of <math>K/\mathbb{Q}_p</math> is induced from a finite field extension <math>\mathbb{F}_{p^n}/\mathbb{F}_p</math>.
