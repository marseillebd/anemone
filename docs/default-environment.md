# Anemone's Default (Primitive) Environment

This document describes the environment that Anemone should provide at the top-level, even in the absence of any user-written Anemone code.
They provide access to the primitive operatives and values that are built-in to Anemone.

Normal use of Anemone should not directly use these, as they are defined intentionally minimally.
Instead, implementations should be able to rely on these primitives so that the standard library can be built in portable Anemone.
This way, a single pure-Anemone standard library implementation can be re-used across all Anemone implementations.

## Expanded or Restricted Top-Level Environments

Anemone implementation may provide additional primitives in the top-level beyond those listed in this document.
To avoid name clashes, it is recommended that implementations:
  * only add bindings in the `value` and `module` namespaces, or a vendor- and/or implementation-specific namespace
  * names added in the `value` or `module` namespaces should be prefixed with double underscores (`__<name>`)
  * A vendor-specific namespace should take the form `__vendor-<name>`, where `name` may be a recognizable name of
    1) an organization or individual (who invents a new general-purpose primitive),
    2) a standards body or standards track (which attempts to unify competing primitive semantics).
  * An implementation-specific namespace should take the form `__builtin-<impl>`, where `impl` may be
  * A namespace both vendor and implementation-specific should take the form `__vendor-<name>-<impl>`
A vendor-specific namespace may be the name of a standards body (as competing , an organization or individual, or an implementation.


It is recommended that Anemone users wishing to access vendor- or implementation-specific functionality do so through a library module(-tree) (which therefore should be provided by the vendor) preferably placed under the `Vendor` or `Builtin` modules (i.e. environments bound in the default environment's `module` namespace with those names).
If there is no chance of confusion, such modules may also be placed in the default environment, but note the wide array of potentially confusing name choices:
  * common words
  * programming jargon
  * the names of programming languages
  * jargon from other fields of study (you might not know that field's jargon or even that the field even exists)
  * names of historical or mythological events or figures (which often get used to name new languages)
  * various sorts of naming conventions that I couldn't think of off the top of my head
The key idea is this: **whether different bits of code integrate with each other should not depend on the amount of clout (monetary, political, meritocratic, &c) an individual or organization happened to possess at some particular point in history**.
The vendor should expect have full control over their modules, so users should not attempt to define sub-modules (or make any other definitions) within a vendor's module-tree.

Certain specialized Anemone implementations may remove or re-define some primitives from this environment.
For instance, an implementation backing an interactive "Try Anemone" site might remove a `__stdin__` primitive, or re-define it so that attempts to use it always result in an error.
Portable Anemone code need not consider such implementations, as they are (read: should be) niche, and users of such implementations should expect general-purpose code to possibly fail.

## Core Features

### `($__lambda__ (,parameter ,@parameter) ,sexpr:body)`

Supporting syntax:
```
parameter ::= ,symbol      ⇒ strict
           |  (~ ,symbol)  ⇒ lazy
```

Ah, the classic λ!, with this, Anemone is already Turing-complete!

`__lambda__` is an operative with creates closure values.
The resulting closure's static environment is just the current environment.
It does not set the name of the closure, but does set the location as the location of the operative call.


A closure understands that its parameters are either strict or lazy.
When an argument is passed to a closure whose next parameter is strict, the argument is evaluated before passing (note that thunks are first-class, and so if an argument evaluates to a thunk, it is not forced).
However, when the next parameter is lazy, the argument expression is suspended (a thunk value with the call site's environment is created to wrap the argument expression).

Recall that every function in Anemone takes exactly one argument.
This is why `parameters` cannot be empty.
The allowance for defining multiple parameters at once is so that implementations can (as an optimization) implement closures that aggregate multiple curried arguments and create a single callee environment for all of them, rather than a new environment for each argument.

### `(__eval__ env sexpr)`

Ah, the magic of eval!, and now we have a Lisp!

`__eval__` is a function which takes an environment and an s-expression.
It evaluates the s-expression in the environment, and returns the result.
Control operations escape from invocations of eval.

### `(__force__ value)`

Since we have first-class thunks, we also need a way to force these thunks.

`__force__` is a function of one argument, which may be of any type.
If the argument is a thunk, the thunk is evaluated, then the result saved into that thunk before returning it.
On subsequent forcings of the thunk, the saved result is returned directly.
Any argument other than a thunk is returned un-altered.

Note that it expected for thunks to contain pure code.
This is what enables us to safely save the result if/when the thunk is forced again.
It also allows implementations to ignore issues of concurrency: since a pure expression always evaluates to the same value and has no side-effects, a pure thunk may be simultaneously forced by multiple threads without coordination.

**Warning**: If a thunk's suspended code is not pure, then the user (owner) of that thunk should ensure it is forced at most once.

If alternate semantics are desired (e.g. do not save the result, lock the thunk during evaluation, begin evaluation in a new thread immediately on creation, and so on), these can be implemented as Anemone libraries, using various types of reference cell.

## Sequential Programming

### `($__sequence__ ,@sexpr:stmt)`

Evaluate each statement in-order and reduces to the result of the last.
If there are no statements, then just reduces to unit.

### `($__defineHere__ ,symbol:ident ,sexpr:body)`

Evaluate `body` to a value, and bind it to `ident` in the the `value` namespace of the current environment.
Reduces to the value of the body.

I think this could be defined in terms of `__define-in__`, but without a way to pattern-match s-expressions, it would be more tedium than it's likely worth.
Thus, I've included it in the default environment
For reference, it would be equivalent to:
```
$__lambda__ (env loc `(,<symbol:ident> ,<sexpr:body>)):
  __define__ env (__sym-intro__ "value") ident (__eval__ env body)
```

## Booleans

### `__true__` and `__false__`

The two boolean values

TODO I suppose I could eliminate these with `$__defineHere__ __true__ {0 __equal__ 0}` and `$__defineHere__ __false__ {0 __equal__ 1}`

### `($__cond__ ,@arc)`

Supporting syntax:
```
arc ::= (,sexpr:predicate ,sexpr:consequent)
```

Each arc is visited in-order:
  * The `predicate` expression is evaluated to a boolean value
  * If that predicate is true, then the result of the expression is the evaluation of the `consequent`
If none of the arcs' `predicate`s are true, then the result is the unit value.
Note that a `__cond__` with zero arcs is allowed, and will always evaluate to unit.

### `(__equal__ a b)`

Returns `__true__` if `a` and `b` are equal, `__false__` otherwise.

This function only provides access to equality for a few built-in types:
  * unit
  * booleans
  * integers
  * strings
  * symbols
  * lists -- TODO possibly
  * locations
  * sexprs -- TODO possibly, and probably ignoring the location
  * type constructors
  * environments -- TODO very stretch, much ehhhhhg, though the test would be fast
  * Prompts -- TODO I need to work this out though, probly similar to types vs. tycons


If the types of the arguments differ, the result is always `__false__`.
If the type of either argument is not on the above list, the result is always `__false__`.

TODO The idea here is to perform equality on small types (types that don't require traversal)
Equality on types is out, because that uses a unification algorithm.
Equality on function types is out because of the intentinoal/extensional problem.
I've not done equality on thunks because they are something like functions, but (TODO) I may decide to allow it.

TODO I'm debating having equality on lists and sexprs.
Lists probably I should just do it for speed.
For sexprs though, the question is whether I should also check the locations for equality; my instinct is not.
Doing either clashes with the "quick in-and-out" philosophy, but maybe that philosophy is bunk.

## Arithmetic

### `(__add__ a b)`, `(__sub__ a b)` TODO

## Lists

### `($__list__ ,@sexpr)`

Create a list value whose elements are the evaluation results of the s-expressions.

This syntax is targeted by z-expression square-bracket syntax.

### `(__cons__ x xs)`

Create a new list which has the element `x` pushed on the from of `xs`.

### `(__uncons__ lst onNil onCons)`

If the list `lst` is empty, evaluate `onNil ()`.
Otherwise, `lst` must be of the form `(__cons__ x xs)`; evaluate `onCons x xs`.

## S-Expressions TODO

S-expressions are abstract types in Anemone, which is an unusual choice for a lisp.
Other lisps (that I know of) treat s-expressions as equivalent to various types (integers
That is to say, the injections
  from values of type integer, string, symbol, and list of s-expressions
  into values of type s-expression
  is implicit.
In anemone, the injection must be explicitly made.
This allows the representation of s-expressions by the interpreter to be altered without affecting user code.

I also believe that it makes it much more difficult to confuse the object-language with the meta-language.
In languages as meta as lisps, explicit injections are likely to detect bugs due to this confusion.
In any case, these primitives will likely only be used in quasiquotation and similar syntaxes, and mostly not used directly.

### `(__sexpr-intro__ x)`

Call with an integer, string, or symbol to create the corresponding atom.
Call with a list of s-expressions to create a combination.

### `(__sexpr-elim__ sexpr onInt onStr onSym onCombo)`

Calls one of the `on*` functions based on the construction of the `sexpr`.
  * evaluate `(onInt n)` if `sexpr` is an integer atom containing `n`
  * evaluate `(onStr s)` if `sexpr` is a string atom containing `s`
  * evaluate `(onSym x)` if `sexpr` is a symbol atom containing `x`
  * evaluate `(onCombo sexprs)` if `sexpr` is a combination containing the list `sexprs`

### `(__sym-intro__ str)`

Create a symbol from a string.
All symbols created from the same string are equal.

### `(__sym-elim__ sym)`

Retrieve the string which created the symbol.

## Types

### `(__typeof__ value)`

Return the type of the passed value.

### `(__type-elim__ ty k)`

Eliminating a type provides access to the type constructor and arguments of a type.
The `ty` argument must be a type.
The `k` argument should be a function taking a type constructor and a list of values.
The return value is the return value of `k`.

### TODO type constructor values

`__tycon-unit__` `__tycon-int__`

## Environments

### `(__new-env!__ parent)`, `(__new-emptyEnv!__ ())`

Create a new environment which is either the child of the `parent` environment, or else has no parent.

Despite environments being append-only, each call to one of these functions produces a different value from other ones.
That is, they cannot be naïvely substituted for a variable used multiple times, and are thus not pure.

### `(__lookup__ env namespace ident)`

Retrieve the value bound to `ident` in the `namespace` of the `env` environment.

TODO there's a question of purity here

### `(__define__ env namespace identifier value)`

Binds the `identifier` to `value` in the `namespace` namespace of the `env` environment.
Both `namespace` and `identifier` must be symbol values.
Returns `value`.

Note that Anemone has strict rules about identifiers being shadowed, which allows optimizers to understand the code more fully.
Bindings cannot be overwritten, nor can they shadow bindings from parent environments.
Furthermore, once a binding has been created in a child environment, it is "reserved" in all its ancestors.
Attempting to write a binding into an environment which already has that name reserved is an error, as that would create shadowing.

## Metadata

In most languages, the names and locations describing the definition of certain objects is generated automatically.
In a language as meta as Anemone however, such automatic generation might not do a good job of describing the values they are attached to.

Names consist of a non-empty sequence of name crumbs, where each name crumb is a pair of symbols, one for the namespace and one for the identifier.
While name has is its own independent type, a name crumb is simply a singleton name.
When a name is not a singleton, we call it a name trail.

Locations can be either known or unknown.
Known locations consist of a filename, start line/col, and end line/column.


### `(__name-intro__ namespace ident)`, `(__name-intro__ names)`

Create either a name crumb from two symbols, or conjoin a non-empty list of names into a single name.

### `(__name-elim__ name onTrail onCrumb)`

If the passed name is a trail, call `onTrail` with the list of crumbs that constituted it.
If the passed name is a crumb, call `onCrumb` with the namespace and identifier of the crumb.

### `(__upd-name__ name value)` `(__upd-loc__ loc value)`

Update the name or location of a value.
Note that these are pure updates rather than mutations, so a value may be assigned different names in different contexts.
E.g. it may have a qualified name if accessed through an imported module, but a simple name if the value is imported directly.

The primitive types which support names are:
  * closures,
  * types,
  * environments.

The primitive types which support locations are:
  * s-expressions
  * closures,
  * types,
  * environments.

If a value does not support names or locations, calls to the corresponding update functions return the original value unmodified.
