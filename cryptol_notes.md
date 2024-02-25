Chapter 1.

Everything is bits? Always has been. (Yet, the expressibility is off the
charts!)

The section on type-directed splits is blowing my mind. I can't figure out
exercise 1.24.

The idea of zero: can we do something similar w.r.t initialization of values in
Horace?

Integral types: I'm thinking in our language, integer types either based on
number of bits you want to express (wrapping around for overflow/underflow) or
an explicit range of values (automatically ensuring add/subtracts don't go out
of bounds).

The type inference gets me sometimes. I was expecting this to be some kind of
issue:
```cryptol
[1 : [_] .. 10]
```
where the `_` would be `1` since one bit is required to represent `1`, but it's
`4` since it reads it from the `10`. That also affects things like elements
wrapping around. How does it know?

The example of `tail` is pretty cool, but the failure of this confuses me:
```cryptol
tail True
```
What is the difference between types `[1]Bit` vs. `Bit`? What is `[0]Bit`?

The motto of "Well typed programs do not go wrong" has me wondering a previous
thing we thought about... Can we encode cycle count information into the types?
This is a place where inference is entirely necessary; the programmer can't
runs in a time lower than is humanly (or computerly) possible to implement in
assembly.

Is size polymorphism a good fit for Horace? Size polymorphism + algebraic data
types (or are they really the same thing!?)?

No function composition with `.`?

Exercise 1.55: this seems very easy to solve with a map and fold combo
(preempting a later part of the book I presume). I did not understand the
hint--what is the intended way?

Can't figure out Exercise 1.63, still don't know folds apparently!

Regarding `+` requiring the `Ring` typeclass: going all the way to ring? Don't
even want to stop at group or even monoid or even magma first?

Like the 433 prereq quiz but better:
```
let cmpRing x y z = if x == y then z + z else z
```
Was the `if` expression even covered in detail before this? (I mean well, duh,
you should know it anyway.)

End of Chapter 1. Skipping to Section 4.2 (Establishing correctness).

Starting to hate the use of LaTeX default font. Thin lines are very hard to
read on low DPI monitors, and, of all things, the mathematical equations aren't
typeset in Computer Modern (argh!!).

Want to do Exercise 4.12 but can't do it. Need to put in more effort!

End of Section 4.2. Skipping to Appendix A (Solutions to selected exercises).

I did not think this book would have answers to the exercises. Otherwise I
would have totally cheated and looked things up.

Solution to Exercise 1.55: Wow!! so smart!! Using the type polymorphism in a
major way! That is actually really cool

Solution to Exercise 1.63: is there a difference between multiple `,` and `|`
in list comprehensions? Was this in Haskell?

4.12: could have gotten this had I had a little more experience and didn't skip
down here so fast
