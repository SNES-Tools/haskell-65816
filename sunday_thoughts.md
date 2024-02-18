# A name for the language

Ben Titzer from CMU created a systems language called
[Virgil](https://github.com/titzer/virgil). I was first introduced to this
language last summer actually at the Marquette University REU, as that was one
of the projects going on during that time. As a (former) classical Latin
scholar the name stuck out to me (in particular, the fact that the name is
[misspelled](https://en.wikipedia.org/wiki/Virgil#Spelling_of_name)). In that
spirit I propose the language be called *Horace* (I was even thinking to be
more of an asshole to call it *Horice* to get back at somebody I have no beef
with at all).

### Regarding sprite types

I think these are okay. Really, they're just structs, and it's fine to have a
struct allocated on the stack or in static areas because we know the size. The
tough part is if these sprite types (which we assume to be really wide) will be
passed by value to functions. I'm not entirely clear yet on how functions
should act (knowing that we ought to inline many for efficiency sake).

I was also a little skeptical about how exactly one would initialize sprite
types, and how such sprites could be reinitialized (especially thinking about
lifetimes and how sprites could be passed around). I think we should allow for
some "reinitialization" of sprites on top of the existing allocation.

### Pass by value

I think I'm going to be *against* pass by value, maybe even for all types! What
strange decisions we will be having throughout the design phase! (This makes a
lot of sense if functions are really just macros inlined everywhere, which is
what I'm learning towards sometimes--try to argue against me on this).

A simple thing to think about: calling a function by a literal, like
```
foo(1)
```
Breaks my dream of no pass by value.

I'm thinking now that we could split up callable units into *functions* and
*subroutines*, where a function should be a pure computation (can inline,
statically resolve, turn into lookup tables), versus subroutines, which should
involve the updating of state. But what would be the use cases?

### The inclusion of array types

I think we should allow the user to have array types. It's essentially a
syntactic rolling up of many variables.

### Absolutely no arbitrarily-sized types or dynamic allocation

We should prohibit any arbitrarily-sized types or dynamic allocation (why?).

### On implementation influencing language design

As I've been thinking of language features and how things "should look", I've
been somewhat conscientious about how I would go about implementing these
things. To some extent the implementation side has been punted, i.e. define
nice abstractions to the programmer and we'll figure out how to translate it
into efficient assembly. But I argue that the target SNES should leave some
footprint on the features of the language design, for two reasons:
  1. From a practical perspective, I will eventually have to implement these
     features, and there's not a lot of time to do that, so there is some value
     in sacrificing the overall beauty of code to make writing the compiler a
     little easier.
  2. From a principle perspective, to design a game programming language
     orthogonal to the system we are targeting would defeat the purpose of this
     assignment (but could increase its generality!).

### The stack is not your friend

Everyone loves to have values on the stack. But you rarely see meaningful
variables stored on the stack in SNES games. There are a few reasons:
  1. The stack is pretty small. On the SNES the stack area is at most 8 KB
     (addresses $00:0000 to $00:1FFF). However, some of that space would be
     nice to use for static area since (as I explain a little more later) it's
     more efficient to put stuff there.
  2. Addressing the stack is even smaller. You are allowed only an 8-bit offset
     from the stack pointer, so things have the potential to get out of hand.
     The common case might not lead to issues, but we can't go crazy with it.
  3. SNES developers came from the NES, which used a 6502 CPU that had no
     stack-based addressing. Additionally, it was common practice with the
     limited NES RAM (2 KB!) to bound the stack at just 96 bytes, enough for a
     few return addresses and temporary storage for values in registers.
  4. Addressing the stack loses 1 cycle compared to zero page storage (and ties
     absolute addressing). You also lose many possible addressing modes which
     can be done directly from the zero page by having to use the stack
     addressing mode instead.

The assumptions we are making in the language (static allocation of state--and
we hope most of the program variables live here) should allow for good use of
static area and thus efficiency gains!
