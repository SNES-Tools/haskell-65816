import Data.Word16
import Data.Word32
import Data.Word8

{-

The reference for 65816 instructions is at
http://www.6502.org/tutorials/65c816opcodes.html

Regarding pretty printing: should the compiler generate assembly code that we
can run through a 65816 assembler, or should we just go ahead and generate the
binary directly? (This would require more work and I'm not sure we would get
extra benefit from this.)

-}
{-
This type holds all possible opcodes in 65816, and labels.
-}
data Instruction
  = ADC Operand
  | SBC Operand
  | CMP Operand
  | CPX Operand
  | CPY Operand
  | DEC Operand
  | DEX
  | DEY
  | INC Operand
  | INX
  | INY
  | AND Operand
  | EOR Operand
  | ORA Operand
  | BIT Operand
  | TRB Operand
  | TSB Operand
  | ASL Operand
  | LSR Operand
  | ROL Operand
  | ROR Operand
  | BCC String
  | BCS String
  | BEQ String
  | BMI String
  | BNE String
  | BPL String
  | BRA String
  | BVC String
  | BVS String
  | BRL String
  | JMP String -- does not capture all addressing modes
  | JSL String -- does not capture all addressing modes
  | JSR String -- does not capture all addressing modes
  | RTL
  | RTS
  | BRK Operand
  | COP Operand
  | RTI
  | CLC
  | CLD
  | CLI
  | CLV
  | SEC
  | SED
  | SEI
  | REP Operand
  | SEP Operand
  | LDA Operand
  | LDX Operand
  | LDY Operand
  | STA Operand
  | STX Operand
  | STY Operand
  | STZ Operand
  | MVN Operand Operand
  | MVP Operand Operand
  | NOP
  | WDM Operand
  | PEA Operand
  | PEI Operand
  | PER
  | PHA
  | PHX
  | PHY
  | PLA
  | PLX
  | PLY
  | PHB
  | PHD
  | PHK
  | PHP
  | PLB
  | PLD
  | PLP
  | STP
  | WAI
  | TAX
  | TAY
  | TSX
  | TXA
  | TXS
  | TXY
  | TYA
  | TYX
  | TCD
  | TCS
  | TDC
  | TSC
  | XBA
  | XCE
  | Label String -- here is the label
  deriving (Show)

{-
These are all the possible operand types. Note that the use of a specific
operand types
-}
data Operand
  = Imm8 Word8
  | Imm16 Word16
  | Abs Word16
  | AbsX Word16
  | AbsY Word16
  | Dir Word8
  | DirX Word8
  | DirY Word8
  | Accumulator -- only used to specify no operand in INC and DEC
  | DirInd Word8
  | Long Word32
  | LongX Word32
  | DirIndLong Word8
  | DirIndY Word8
  | DirXInd Word8
  | AbsInd Word16
  | AbsIndLong Word16
  | AbsXInd Word16
  | DirIndLongY Word8
  | Stack Word8
  | StackInd Word8
  deriving (Show)
{-
Addressing addressing modes:

There are many addressing modes, and many instructions that take operands only
support a few of the addressing modes. I'm not sure if we can assert only
correct addresing modes in the types (without having multiple copies of the
same constructor lying around) or if we must throw runtime errors for
misused addressing modes.

Additionally, some instructions imply the operand through different mnemonics
(for example, PHA, PHX, PHY, etc. for pushing to the stack). We could possibly
change the Haskell representation for a single push and expose the operand as
an parameter on the constructor (so it would be like PUSH A, PUSH X, etc.).

-}
