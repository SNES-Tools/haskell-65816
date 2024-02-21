import Prettyprinter
import Prettyprinter.Render.Text
import System.IO

{-
  This is a test implementation for pretty printing with a simple fake
  assembly.
-}
-- dummy types
data Program =
  Code [Instr]

data Instr
  = Add Operand
  | Sub Operand
  | Label String
  deriving (Show)

data Operand
  = One Integer
  | Two String
  deriving (Show)

instance Pretty Program where
  pretty (Code is) = vsep (map pretty is)

instance Pretty Instr where
  pretty (Add o) = indent 4 (group (pretty "Add" <+> pretty o))
  {- is this the right place for indent? assembly programs don't really lend
     themselves to the kind of "block" layout which I think pretty printers are
     really effective towards. Labels, which don't get indentation, don't
     necessarily own the code under it (unless we try to enforce nested
     labels, which asar supports).
   -}
  pretty (Sub o) = indent 4 (group (pretty "Sub" <+> pretty o))
  pretty (Label s) = group (pretty s <> pretty ":")

instance Pretty Operand where
  pretty (One x) = pretty x
  pretty (Two s) = pretty s

exampleCode :: Program
exampleCode =
  Code
    [ Add (One 50)
    , Sub (One 25)
    , Label "foo"
    , Sub (Two "foo")
    , Label "bar"
    , Add (One 10)
    , Sub (One 20)
    , Add (Two "bar")
    ]

putDocCompact :: Doc a -> IO ()
putDocCompact = renderIO System.IO.stdout . layoutCompact
