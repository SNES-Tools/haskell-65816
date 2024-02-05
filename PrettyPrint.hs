data Doc -- the constructors are not public to the user
  = Nil
  | Text String Doc
  | Line Int Doc
  | Union Doc Doc

-- monoid?
(<>) :: Doc -> Doc -> Doc
(<>) doc y =
  case doc of
    Text s x -> Text s (x Main.<> y) -- x is a Doc
    Line i x -> Line i (x Main.<> y)
    Nil -> y
    Union x z -> Union (x Main.<> y) (z Main.<> y)

-- these functions are the API (?) which makes the implementation opaque
nil :: Doc
nil = Nil

text :: String -> Doc
text s = Text s Nil -- simple conversion

line :: Doc
line = Line 0 Nil -- 0 nesting?

nest :: Int -> Doc -> Doc
nest i doc =
  case doc of
    Text s x -> Text s (nest i x)
    Line j x -> Line (i + j) (nest i x)
    Nil -> Nil
    Union x y -> Union (nest i x) (nest i y)

layout :: Doc -> String
layout doc =
  case doc of
    Text s x -> s ++ layout x
    Line i x -> '\n' : replicate i ' ' ++ layout x
    Nil -> ""

{-
Given a document, representing a set of layouts, group returns the set with one
new element added, representing the layout in which everything is compressed
on one line.
-}
group :: Doc -> Doc
group doc =
  case doc of
    Text s x -> Text s (group x)
    Line i x -> Union (Text " " (flatten x)) (Line i x) -- two options here
    Nil -> Nil
    Union x y -> Union (group x) y -- left only? an invariant I don't understand

-- w total width
pretty :: Int -> Doc -> String
pretty w doc = layout (best w 0 doc) -- 0 = no chars on first line

(<|>) :: Doc -> Doc -> Doc -- union of two sets of layouts
(<|>) x y = Union x y

flatten :: Doc -> Doc
flatten doc =
  case doc of
    Text s x -> Text s (flatten x)
    Line i x -> Text " " (flatten x) -- same as above except no union?
    Nil -> Nil
    Union x y -> flatten x -- also due to the invariant

{-
Next, it is necessary to choose the best among the set of possible layouts. This
is done with a function best, which takes a document that may contain unions, and
returns a document containing no unions.

params:
- the available width w
- the number of characters k already placed on the current line
-}
best :: Int -> Int -> Doc -> Doc
best w k doc =
  case doc of
    Nil -> Nil
    -- for a newline it is set to the indentation
    Line i x -> Line i (best w i x)
    -- for text it is incremented by the string length
    Text s x -> Text s (best w (k + length s) x)
    -- For a union, the better of the best of the two options is selected
    Union x y -> better w k (best w k x) (best w k y)

better :: Int -> Int -> Doc -> Doc -> Doc
better w k x y =
  if fits (w - k) x
    then x
    else y

fits :: Int -> Doc -> Bool
fits w doc =
  if w < 0 -- no available width remaining for anything else
    then False
    else case doc of
           Nil -> True
           -- if the document begins with text then it fits if the remaining
           -- document fits in the remaining space
           Text s x -> fits (w - length s) x
           Line i x -> True -- always true?... document begins with newline!

{-
TREE EXAMPLE!!
-}
data Tree =
  Node String [Tree]

showTree :: Tree -> Doc
showTree (Node s ts) = text s Main.<> nest (length s) (showBracket ts)

showBracket :: [Tree] -> Doc
showBracket [] = nil
showBracket ts = text "[" Main.<> nest 1 (showTrees ts) Main.<> text "]"

showTrees :: [Tree] -> Doc
showTrees [t] = showTree t
showTrees (t:ts) = showTree t Main.<> text "," Main.<> line Main.<> showTrees ts

showTree' :: Tree -> Doc
showTree' (Node s ts) = text s Main.<> showBracket' ts

showBracket' :: [Tree] -> Doc
showBracket' [] = nil
showBracket' ts =
  text "[" Main.<> nest 2 (line Main.<> showTrees' ts) Main.<> line Main.<>
  text "]"

showTrees' :: [Tree] -> Doc
showTrees' [t] = showTree t
showTrees' (t:ts) =
  showTree t Main.<> text "," Main.<> line Main.<> showTrees ts

tree :: Tree
tree =
  Node
    "aaa"
    [ Node "bbbbb" [Node "ccc" [], Node "dd" []]
    , Node "eee" []
    , Node "ffff" [Node "gg" [], Node "hhh" [], Node "ii" []]
    ]

testTree :: Int -> IO ()
testTree w = putStrLn (pretty w (showTree tree))

testTree' :: Int -> IO ()
testTree' w = putStrLn (pretty w (showTree' tree))
