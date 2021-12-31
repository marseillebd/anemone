# Append-Only Environments

The usual translation from a substitution-based calculus to an environment-basd calculus requires environments to be immutable once created.
However, the straightforward way to implement recursion in a strict language is to mutate the environment when a new binding is created: this way the body of the definition can mention the undefined variable (assuming it is protected by being inside a function or otherwise protected by not yet being evaluated), and trust that that correct binding will be in the environment by the time the variable is used (i.e. evaluated).
This is much easier than implementing a combinator for mutual recursion and re-writing recursive definitions to use it instead of begin defined straightforwardly.

With macros that are only computed at "compiletime" (i.e. are not first-class), the mutable implementation is indistinguishable from naïve substitution.
With fexprs, the assumptions that make the technique safe break down.
We cannot perform substitution without evaluating every fexpr, because an fexpr may introduce unexpected bindings at "runtime".
That is, if an optimizer makes a substitution, an fexpr that the optimizer did not reduce may invalidate the earlier substitution by shadowing the original binding.

The key idea is to recognize that in (almost?) every language, shadowing is already unadvised, and comes with compiler warnings (if applicable).
I propose that in the presence of fexprs, we _error_ on name shadowing rather than simply warn.
This allows us to implement environments with the mutation strategy, but with a slight difference:
  * an existing binding in an environment cannot be mutated (disallow redefinition)
  * a binding cannot be made in an environment if a parent environment already binds the same name (disallow shadowing)
By the first rule, the bindings that an environment directly contains (i.e. ignoring parents) are append-only (thus the name of the implementation strategy).
By the second, it is impossible to tell if a binding was made directly in an environment or in one of its parents; this is essential to maintain the semantics of substitution, which has no notion of parent/child scope.


There is one wrinkle to this strategy: fexprs may not be evaluated in-order.
That means that an fexpr that appends to a child environment may be evaluated before an fexpr that appends to a parent environment.
If this were allowed to happen, we may end up with shadowing anyway, destroying our equivalence with substitution!
To combat this, we must ensure that when a name is defined in a child environment, not only is it not _currently_ defined in a parent, but also that it will _never_ be defined in a parent (without causing an error condition).
I propose that when a binding is made in an environment, the same name is also _reserved_ in all its parents.
A name may be reserved multiple times, but if an attempt is made to bind a reserved name, that results in the same error as if attempting to re-bind a name or shadow an existing binding.
