module AST where

type Identifier = String -- placeholder, there will be some rules for identifiers

type TypedIdentifier = (Identifier, Type)

{- anytime a variable/constant is introduced, you would have to write its type:
  something like
    x: byte
-}
{-
  I'm thinking of an expression oriented language:
  EXPR := { EXPR* }     -- EXPRs separatd by newlines
        | ()            -- void value!
        | INT           -- casted to width based on how it is used
        | if PRED then EXPR -- value if false?
        | if PRED then EXPR else EXPR   -- susceptible to dangling else!
        | ID <- EXPR    -- assignment (both global state and local vars)
        | var ID: TYPE = EXPR   -- mutable local variable, lexically scoped in block?
        | ID(EXPR*)     -- comma separated, function calls
        | for ID = EXPR to EXPR do EXPR
        | case EXPR of CASES
  CASES := ...
  I don't think it makes sense for functions to be first class...
  * the extra level of indirection could slow things down (but also that's our
    problem to deal with, so maybe it's okay)
  * we will probably want to inline as much as possible
  * update: it might make sense to allow same function pointers as C sometimes
  Missing things:
  * Every n frames
  * (EXPR) for setting precedence order (will the parser do this before AST?)
-}
data Expression
  = Block [Expression]  -- chains multiple expressions together
  | Void -- i think we will have many voids showing up
  | Literal Int
  | If Predicate Expression
  | IfElse Predicate Expression Expression
  | Assignment Identifier Expression
  | Variable TypedIdentifier Expression -- all variables must be initialized
  | FunctionCall Identifier [Expression]
  | Iterator Identifier Expression Expression Expression
    -- first two expressions are lower/upper bound (I'm presuming integers only)
    -- third is body of loop
  | Operation IntOperator Expression Expression
    -- also thinking only operation between integral expressions
    -- does not define a proper precedence order
  | Case Expression [(Expression, Expression)] Expression
    -- the pair is value to check against (should be either int or enum literal)
    -- and the value if that case is matched

data IntOperator
  = Plus
  | Minus
  | Times -- this is dangerous to give to the programmer freely
  | Divide -- even more dangerous
  | LeftShift -- shifts of many amounts will be difficult to do (65816 to blame)
  | RightShift  -- arithmetic
  | BitwiseAnd
  | BitwiseOr
  | BitwiseEor

{-
  I'm thinking integer only comparisons here (signed/unsigned?)
-}
data CompareOperator
  = Equal -- comparisons to 0 are a common thing, so should be optimized
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual

{-
  I don't want there to be a boolean type, instead you get predicates, which are
  like expressions, except they can only be used in certain places, and this
  affects the control flow.

  Since if is an expression, this means something like
    x <- if y = 2 then 1 else 0
  is not too unusual... (as I think of it, this is how boolean _values_ would
  have been done under the hood)

  PRED := (PRED)
        | not PRED
        | PRED and PRED
        | PRED or PRED
        | EXPR COMP EXPR
  COMP := '='
        | '/='
        | '<'
        | '<='
        | '>'
        | '>='
-}
data Predicate
  = Boolean Bool
  | Not Predicate
  | And Predicate Predicate
  | Or Predicate Predicate
  | Comparison CompareOperator Expression Expression

{-
  The init, loop (see below), and each function gets an expression to be
  evaluated. As it stands right now, I don't think there's a difference
  syntactically on what kinds of expressions are allowed in these three
  usages.

  However, we will have to think about rules on scoping, especially global state.
-}
{-
  This mode definition is pretty analogous to the structure of the sample
  pong code
-}
data Mode = Mode
  { modeGraphics :: [Graphics]
  , modePalettes :: [Palette]
  , modeConstants :: [(TypedIdentifier, Expression)]
      -- the value of an expression must be statically computable
  , modeState :: [(Identifier, Type)]
  , modeInit :: Expression -- inits that have params?
  , modeLoop :: Expression
  , modeFunctions :: [(Identifier, [TypedIdentifier], Type, Expression)]
      -- function name, parameters, return type, body
  }

type Graphics = () -- placeholder

type Palette = () -- placeholder

type AnimationFrame = () -- placeholder

{-
  I think I have an idea on how to unify all these things (Graphics, Palette,
  AnimationFrame) together for Sprites:

  What I was thinking before is that you'd have all the graphics in one file,
  which would be declared in the `graphics` section, and a set of palettes
  that you might want to use for the sprite. These would be located in external
  files.
  The animation frames would be where you build the sprite frames based on the
  graphics/palettes you allocated.

  But I think we can just get rid of the middle man in the graphics section and
  have each animation frame be its own externally defined file with just that
  frame and we would figure out how to put those graphics together.

  However, things are a little more complicated for background layers. I think
  of them as an array of tiles (each tile one graphic) that are then stitched
  together to make the full background. Instead of sprites where you would
  write how to stitch graphic elements together at compiletime, background
  layers should be modified at runtime (think in Mario when Mario breaks a
  brick--the background must update).

  So I think we should handle these two cases differently. (This is a very
  confusing explanation on paper; I think I have a more concrete idea in my
  mind.)
-}
{-
  I haven't fully decided on the types yet. I think we should avoid pointer
  things (we'll be presumably moving memory around and pointers are annoying
  and dangerous in general). But no boolean type. Enum types are okay, because
  they can be integers under the hood (boolean enum?).

  Having variables of sprite types also made us think about dynamic memory
  allocation. I don't think I really want there to be dynamic memory allocation
  in the language. Perhaps there are ways to alleviate this.
  THE SUNDAY THOUGHTS HANDLE THIS SOMEWHAT!
-}
data Type
  = ByteType
  | WordType
  | SpriteType Identifier
  | EnumType [Identifier]
  | VoidType  -- void type

data Sprite = Sprite
  { spriteGraphics :: [Graphics]
  , spritePalettes :: [Palette]
  , spriteAnimationFrames :: [AnimationFrame]
  , spriteConstants :: [(TypedIdentifier, Expression)]
      -- the value of an expression must be statically computable
  , spriteState :: [(Identifier, Type)]
  , spriteInit :: Expression -- inits that have params?
  , spriteLoop :: Expression
  , spriteMethods :: [(Identifier, [TypedIdentifier], Type, Expression)]
      -- function name, parameters, return type, body
  }
{- Who is allowed to call a sprite method? I'm thinking mode only, but there
can be a lot of dependencies between functions in the mode/having other sprites
as state.
-}
