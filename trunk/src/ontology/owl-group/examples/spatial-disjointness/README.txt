The 3 OWL files in this directory illustrate issues with error
checking in BFO2. Each OWL file has a corresponding screenshot.

They should be viewed in the following order:


sd-atemporal.owl
----------------

This models spatial disjointness using atemporal properties, as in
RO. i.e. we have transitive part_of and has_part that stand in an
inverseOf relationship.

The pattern used is a standard one, as found in GO and Uberon

    (part_of some X) DisjointWith (part_of some Y)

See:

  http://purl.obolibrary.org/obo/uberon/references/reference_0000018

In this case we make nucleus and mitochondrion spatially disjoint
using the following GCI:

   [1]  (part_of some mitochondrion) DisjointWith (part_of some nucleus)

For a temporal reading, we can assume that the above axiom holds for
any snapshot / instant of time t. Although uncommitted to a temporal
reading as yet, the desired axiom is:

   all t, not exists x,m,n : part_of(x,m,t),part_of(x,n,t),inst(m,mt,t),inst(n,nucleus,t)

The OWL is an approximation of this. Does it work?

The reasoner finds that mitochondrion is unsatisfiable. This is the
desired behavior, as we deliberately made an error assuming that every
chromosome was part of a nucleus (in fact mitochondria have
chromosomes too).

(this is a nice example of why inverseOf axioms are useful even for
class-level reasoning)

sd-bfo2-fail.owl
----------------

In migrating from a temporal/snapshot model to BFO2 we have to decide
how to translate spatial disjointness axioms.

In this model we replace the atemporal part_of with
part-of-continuant-at-all-times. This gives us the following GCI:

  [2]
  ('part of continuant at all times' some mitochondrion) 
     DisjointWith
  ('part of continuant at all times' some nucleus)

Note that the FOL reading of this is weaker than we intend. If we have
4 facts:

  * part_of(x,m,t)
  * part_of(x,n,t)
  * instance_of(m,mitochondrion,t)
  * instance_of(n,nucleus,t)

These are consistent. This is not the desired outcome.

But what about at the OWL class axiom level? Do we get the desired
behavior if we don't care about instances?

sd-bfo2-fail contains the same modeling error translated to BFO2:

  every chromosome part-of-continuant-at-all-times some nucleus

We also include an axiom:

  every mitochondrion has-part-at-all-times some nucleus

As an ontology editor, I would like mt to be flagged as unsatisfiable,
as in the atemporal example. However, axiom [2] is too weak for
this. Neither HermiT nor Fact++ finds any class to be unsatisfiable.

sd-bfo2-succeed.owl
-------------------

This is a variant of the above example, but here we write 4 GCIs for
every time we want to say that two structures are spatially disjoint
at any given time t.

  [2] 'part of continuant at all times' some mitochondrion DisjointWith 'part of continuant at all times' some nucleus
  [3] 'part of continuant at all times' some mitochondrion DisjointWith 'part of continuant at all times that whole exists' some nucleus
  [4] 'part of continuant at all times that whole exists' some mitochondrion DisjointWith 'part of continuant at all times that whole exists' some nucleus
  [5] 'part of continuant at all times' some nucleus DisjointWith 'part of continuant at all times that whole exists' some mitochondrion

In this particular example it is axiom [5] that yields the desired
unsatisfiability (see explanation screenshot). However, we need to
write all 4 variants if we are to "defend" ourselves against possible
errors.

The recommendation for BFO2 users want to write spatial disjointness
of analagous axioms is for all 2x2 forms to be written out
explicitly. Note that without additional tool support (e.g. shortcut
relations), this is both tedious and error prone.

The end result is still weaker than desired, as it cannot find errors
in whatever the BFO2 OWL equivalent of the following facts are

  * part_of(x,m,t)
  * part_of(x,n,t)
  * instance_of(m,mitochondrion,t)
  * instance_of(n,nucleus,t)

Conclusions
-----------

Ontology constraints such as spatial disjointness axioms are extremely
useful for detecting errors in ontologies. These have been
particularly effective in GO and Uberon, where they have been used
with RO type atemporal relations.

BFO2 is less useful for finding these kinds of errors. It is necessary
to write 4 axioms instead of 1 to achieve the same "level of
defense". There may be other problems lurking when we compose
relations together.

This result should not be surprising. We can expect consequences like
this when we embed complex FOL into object properties, where the FOL
is opaque to OWL reasoners.
