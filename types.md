# Thoughts about the type system

## The syntax is a placeholder

But should be clear enough to get the point across.

## No type inference

For variable and constant declarations.

## Integral types

There are two types of integers: *range* and *bits*

### Bit types

The bit types simply denote the number of bits that will be used to store the
value. One can declare an integer width from 0 to 32 bits either as signed or
unsigned:

```
var x: unsigned[8] = 255    -- is initialization required?
var y: signed[8]   = -1
```

Negative values are stored bitwise using two's complement.

Arithmetic (`+` or `-`) over this type is performed modulo *2^n* where *n* is
the width in bits. Therefore, the value of the expression
```
{
  var x: unsigned[8] = 255
  var y: unsigned[8] = 1
  x + y
}
```
is 0.

There are the usual equality comparisons (`=` and `/=`). The bitwise operators
`&`, `|`, and `^` are also supported.

The usual ordering comparisons can be performed (`<`, `<=`, `>`, `>=`).
Comparisons between signed and unsigned types should be "as you expect". Some
examples:
```
{
  var x: unsigned[8] = 255
  var y: signed[8]   =  -1  -- bit representations are the same
  if x = y then 1 else 0  -- evaluates to 0
  if x > y then 1 else 0  -- evaluates to 1
}
```

### Range types

The range types denote a range of possible integer values. One can declare a
range where the difference between highest and lowest value is no greater than
65,535:

```
var x: range[0, 10] = 10  -- the bounds are inclusive
```

Arithmetic (`+` or `-`) over this type is clamped between the bounds. That
means the value of the expression
```
{
  var x: range[1, 10] = 5
  x + 10
}
```
is `10`.

Note that you should be able to do arithmetic with a value which lives outside
the clamped range:
```
{
  var x: range[50, 100] = 75
  x + 1
}
```

There are no bitwise operations and there are no unsigned comparisons (i.e.
`<`, `<=`, `>`, `>=` act like signed comparisons).

There is a partial order `<=` on the set of all range types.
`range[i, j] <= range[k, l]` if and only if `i <= k` and `j <= l`.

Signed and unsigned bit types could also be converted to ranges. You know what
the default widest range can be based on the number of bits (for example
`signed[8]` can be converted to `range[-128, 127]` for free). However, tighter
bounds could be established in some cases with some analysis.

### Casting between integral types?

I think casting between unsigned and signed bits of the same width should keep
the bits the same. Seems obvious and easy. (Although, another option is to
transform negative signed values to 0 and transform large unsigned values to
the largest signed value.)

Addition and between unsigned and signed bits work exactly the same because of
two's complement, so I don't think we need to do anything special.

The differences in unsigned and signed bits come in (1) comparisons and (2)
widening. Comparisons between unsigned and signed values should work
mathematically:
```
{
  var x: unsigned[8] = 255
  var y: signed[8]   =  -1  -- bit representations are the same
  if x = y then 1 else 0  -- evaluates to 0
  if x > y then 1 else 0  -- evaluates to 1
}
```
Widening of signed values should have sign extension:
```
{
  var x: unsigned[8]  = 255
  var y: signed[8]    =  -1  -- bit representations are the same
  var z: unsigned[16] =   0
  var w: signed[16]   =   0

  z <- x    -- z = 255
  z <- y    -- z = 255 or 65535 (?)
  w <- x    -- w = 255
  w <- y    -- w =  -1
}
```
What if a signed negative value needs to be converted to an unsigned value of
the same width? Does the sign extension to the wider type happen before the
conversion to unsigned? Or does the conversion to unsigned happen first, where
there is no subsequent sign extension?

One situation I have thought about is addition of range types when the ranges
are disjoint:
```
{
  var x: range[ 1, 10] = 2
  var y: range[11, 20] = 15
  x + y
}
```

What should we expect the type of `x + y` to be? What should be its value? One
clear option is that `x + y: range[12, 30]` (simply adding the bounds together)
and that `x + y = 17` as in normal arithmetic. However, I think this defeats
the purpose of the range type. I think ranges should be less about defining the
ranges of a computation (more descriptive in nature) and more about enforcing
clamping (more prescriptive in nature).

As another thought, it may not be a question we have to think about.
Expressions ought to be used somewhere. If it is not, then we don't necessarily
have to care about its type (or even have to perform the computation, for that
matter). Since we will require the programmer to specify types everywhere, this
means we have something to check against. Therefore, using the above example,
we would have
```
{
  x <- x + y    -- x = 10
  y <- x + y    -- y = 17
}
```

What about casting between ranges that are disjoint? For example
```
{
  var x: range[ 1, 10] = 2
  var y: range[11, 20] = 15
  x <- y
}
```
This could result with `x = 10`, but if this were allowed, this could cause
some intuitive behavior with array indexing as seen below.

Another thought to try to mend the range debate is to only allow ranges to be
added to other ranges where we can't guarantee the resulting evaluation
requires a clamp to be performed. In formal terms, the type of an addition
`x + y` is the type of `x`, say `range[n, m]` where the type of `y` must
satisfy `range[-d, d]` where `d = m - n`. In this example:
```
{
  var x: range[ 1, 10] = 2
  var y: range[10, 20] = 15   -- the lower bound is 10, not 11
  x <- x + y
  -- this expression fails to type check: the addition of `y` will always cause
  -- `x + y` to exceed its bounds, because `y > 9 = 10 - 1`
  x <- y + x
  -- the type of `y + x` is now range[10, 20] and should be fine, it's a
  -- question of whether or not this type can be cast down to `range[1, 10]`
  -- which is probably somewhere above this in the document and I've written
  -- too much so I can't find it anymore
}
```

At the end of the day (today is Sunday), it seems that implicit casting is
going to be a problem and might "smooth out" a lot of issues that should have
otherwise been errors to the programmer.

* with no sophisticated type system, programmers couldn't possibly be aware
  (i.e. automatically warned) that their arithmetic is going to be wrong
* the goal with a sophisticated type system is to guarantee that it will never
  be wrong by rejecting programs that abuse types
* the problem with all this discussion on implicit casting is that we are
  keeping programs safe but the programmer still doesn't know that the
  arithmetic is going to be wrong (it could even be more wrong than they
  thought)

I think a better approach might look like aggressive use of size polymorphic
functions a-la Cryptol, and being more strict about rules, requiring explicit
conversions between types.

## Array types

Array types are vectors of values of a known length. Both the underlying type
and length of the array must be supplied as part of the type:

```
var xs: array[unsigned[8], 10]  -- by default, an array of 10 0s.
```

### Array indexing

An array is always indexed in bounds. An indexing expression must be compatible
with the type `range[0, n - 1]` where `n` is the length of the array.

One question this brings up is the following situation:
```
{
  var xs: array[unsigned[8], 3]
  -- an indexing expression for `xs` must be compatible with `range[0, 2]`
  xs[4]
  -- clearly, this is a type violation, and should not compile, but what about
  xs[2 + 2]
  -- ? It seems based on the above, this should be legal

  -- curiously, this makes the following legal:
  var i: range[0, 2] = 2
  xs[i + 2] -- same as `xs[2]`, which may itself be unintuitive behavior
}
```

The cast-and-clamp behavior mentioned in the previous section has some weird
ramifications for array indexing. If any cast can be made from one range type
or another (or even a bit type to a range type), then the compiler could never
tell you an array would be indexed out of bounds, because the index would be
clamped to be in bounds at runtime, which does not let the programmer know that
they are going to be accessing the array incorrectly. In short, I have to think
about this more.

### For loops over arrays

Because array lengths are known, we can do easy loops over arrays:
```
{
  var counter: unsigned[16] = 0
  var xs: array[unsigned[8], 10]
  for x in xs do {
    -- is `xs` mutable?
    counter <- counter + x
  }
  for i: range[0, 9] = 0 to 9 do { -- absolutely no type inference, but an
                                   -- opportunity for some polymorphic iterator?
    -- is `i` mutable?
    xs[i] <- xs[i] - 1
  }
}
```
Or, because we are wizards, use `map`.

## Records

If I'm understanding the reading on row polymorphism correctly, then functions
can be parameterized by record types as long as the record contains fields with
the appropriate names/types that the function requires (perhaps an opportunity
for monomorphization?).

## Sprite types

I have been thinking more about this so-called "row polymorphism" and how this
pertains to our sprite types. Previously, we have made sprites a special
language feature, but I think with a good type system, we can turn sprites into
a record with row polymorphic functions over classes of sprites.

For example, the minimal amount of information a sprite would need to be
displayed is (in Haskell syntax)
```
type Sprite = 
  { x       :: Int        -- \ position where sprite should be displayed
  , y       :: Int        -- / 
  , display :: Animation  -- information on what graphics needed to display
  } 
```
The `Animation` type encodes information on how to build graphics together for
a particular animation frame.

As long as a record includes these three fields and types, we could call a
function
```
displaySprite :: Sprite -> IO ()  -- replace IO with your favorite Monad
```
to show it to the screen.

This could be expanded easily for a sprite switching between multiple animation
frames:
```
data AnimationFrame = Facing Direction
                    | Attacking Direction
data Direction = Left
               | Right
               | Up
               | Down
type Sprite' = 
  { x       :: Int
  , y       :: Int
  , display :: Animation
  , frame   :: AnimationFrame
  }
```
where `sprite.display = lookupFrame sprite.frame` and `lookupFrame` is a
function
```
lookupFrame :: AnimationFrame -> Animation
```

This also gives control back to the programmer of a Mode on when they want
sprites to be displayed. We previously had this idea that all sprite that were
"loaded" (we never really defined this) were to be both displayed and have some
loop function run on it at the same time. But perhaps we might want to have
some sprites loaded but not displayed (like an inactive sprite to be
temporarily hidden or frozen).

Implementation-wise: we will have to think about how this might affect
allocation of objects (in VRAM) to sprites.

## Void

Void type to cover the following case:

```
if 1 = 2 then 5
```

## Algebraic data types

Everyone's favorite feature that they didn't know they wanted.

Allow algebraic data types but forbid recursion (what about mutual recursion?
hard to check?)

I believe this should cover other things we want, like tag unions and enums.
