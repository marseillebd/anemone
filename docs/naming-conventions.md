# Naming Conventions (TODO)

It's common in lisps for `<foo>?` to mark a predicate, but most functions should be pure if possible, and boolean blindness means we should probably not priveledge booleans (instead prefer deconstructors or custom 2-elem data types).
Thus, I'll probably actually use the `?`-suffix for values which have some sentinel to detect (Nothing, nil, NULL, None, &c).

The `!`-suffix I'll probably continue using as normal maybe.
Well, I want it to mark all non-pure functions, not just referentially-transparent functions.
Might not see much of it if I build an IO monad (or collection of monads)

acronyms and initialisms should have only their first letter capitalized

well-known abbreviations can be used, but beware of abbreviation conflicts especially in interfaces
I've worked on a codebase where we had doctors and documents both abbreviated "doc"
If we could (and would) query a big database of abbreviations used, collisions could be avoided (e.g. oh, someone's already using doc for doctor, I guess I'll use "dr" instead

hyphenated words should likely be treated as a single word, because hyphenation is the first step in forming a new compound.
Today, "e-mail" looks like it a word written by someone who always got their secretary to send around a memorandum on paper.
See also, "boot-strap", name-space".

Kebab vs. snake. vs camel:
  I prefer camel as it is easier to hit the shift key than the hyphen key, and snake case requires you to hit both.
  However, not all smushing of words together is the same.
  In particular, variants o essentially the same value should be kebab'd (e.g. if `cp` is a non-recursive file-copy function, `cp-recurseive` or `cp-r` should be used for a recursive variant.

when there are several things which could be named the same, subscripts are a good way to distinguish them.
(This is dependent on the contributors having keyboards set-up to type such characters.)
For example, if a function takes an original list and recurses on it, the original could be named `list₀`, and the recursor named plain `list` (since the recursion is of primary interest, it needs no subscript).
Or, if a function needs the first two elements of a list but does not inspect them, prefer `elem₀, elem₁` to `first-elem, next-elem`
Sub-/superscript names are tricker as Unicode (at time of writing) does not include the full English alphabet (there's no superscript q, subscripts are more sporadic, and capitals of either case are even less well-covered)


Names beginning with double-underscores are reserved: in general, the Anemone spec may choose to begin using such a name for core functionality at any time, and such names may also change without a change in version number.
However, there are subspaces of this naming pattern that reserved specifically for vendor- and implementation-specific uses.
These names are also subject to change without corresponding change in version number (i.e. the are not part of the interface), _unless_ they
also end in double-underscore ("dunder" names for short).
Dunder names can be expected to remain stable.
Identifiers bound in non-top-level environments are outside the scope of Anemone's reservations, but are automatically reserved for the module author.
Identifiers that begin with a single underscore are "recommended private", and generalyl should not be used except in special circumstances (i.e. they are provided for library authors who "know what they're doing" to work around the restrictions of the usual api).
Avoid name mangling, prefer to hide private bindings in local scopes (e.g. let blocks, or do not export from a module).
If access to private names is needed for testing, export those environments with an underscore-prefixed name.


Sometimes a package needs multiple version numbers.
For example, it may make sense for the primary version number to track the public api, but if the library exports and regularly changes various private bindings, then there should likely also be a separate "private" version number.
Thus, changes to the private api can be made without advancing the public version number, and libraries that do not use the private api need no update their version bounds as often.
It is also especially important for libraries that are bindings around a foreign library.
Such libraries may need to adapt a foreign language's conventions (naming, calling, typing, &c) into Anemone's conventions.
The Anemone api should be the primary version number, but the foreign library's version should also be exposed as a separate "foreign" version number.
All version numbers used by Anemone packages should use SemVer, but not all foreign libraries do; thus, you may need to discard the original library's versioning scheme and adopt a new one which abides by SemVer.


===========

what's the difference between an exception and an error?
I suppose errors aren't really meant to be caught normally; perhaps in deep library code, but not regularly
