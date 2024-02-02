import Data.Word (Word8, Word16, Word32)

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
  | BCC Operand
  | BCS Operand
  | BEQ Operand
  | BMI Operand
  | BNE Operand
  | BPL Operand
  | BRA Operand
  | BVC Operand
  | BVS Operand
  | BRL Operand
  | JMP Operand
  | JSL Operand
  | JSR Operand
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

here, ind = indirect
-}
data Operand
  = Imm8 Word8    -- immediate size doesn't change the opcode, but assembler
  | Imm16 Word16  -- still needs to know size due to variable width registers
  | Abs Word16
  | AbsX Word16
  | AbsY Word16
  | Dir Word8
  | DirX Word8
  | DirY Word8
  | Accumulator -- only used to specify no operand in INC and DEC
  | DirInd Word8
  | Long Word32 -- this should really be 24 bits
  | LongX Word32 -- this should really be 24 bits
  | DirIndLong Word8
  | DirIndY Word8
  | DirXInd Word8
  | AbsInd Word16
  | AbsIndLong Word16
  | AbsXInd Word16
  | DirIndLongY Word8
  | Stack Word8
  | StackIndY Word8
  | Label16 String -- location defined
  | Label24 String -- location defined
  deriving (Show)

isValidOperand :: Instruction -> Bool
isValidOperand (ADC operand) = isCommonOp operand
  where
    isCommonOp (DirXInd _) = True
    isCommonOp (Stack _) = True
    isCommonOp (Dir _) = True
    isCommonOp (DirIndLong _) = True
    isCommonOp (Imm8 _) = True
    isCommonOp (Imm16 _) = True
    isCommonOp (Abs _) = True
    isCommonOp (Long _) = True
    isCommonOp (DirIndY _) = True
    isCommonOp (DirInd _) = True
    isCommonOp (StackIndY _) = True
    isCommonOp (DirX _) = True
    isCommonOp (DirIndLongY _) = True
    isCommonOp (AbsY _) = True
    isCommonOp (AbsX _) = True
    isCommonOp (LongX _) = True
    isCommonOp _ = False
isValidOperand _ = True
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
