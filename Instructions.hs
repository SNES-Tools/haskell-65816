module Instructions where

import Data.Word (Word16, Word32, Word8)

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
  | PER Operand
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

I have exposed some confusion around labels, at least regarding the assembler
(asar) that I am used to using. Labels are strictly a standin for addresses.
Since most operand types refer to addresses, labels can be substitued in this
portions as well. Should our compiler try to turn every memory allocation into
a label, and use labels everywhere? That would make it easier to inspect
compiled code.
-}
data Operand
  = Imm8 Word8 -- immediate size doesn't change the opcode, but assembler...
  | Imm16 Word16 -- still needs to know size due to variable width registers
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
  | Label8 String -- labels for the purpose of (not long) branches
  | Label16 String -- location defined
  | Label24 String -- location defined
  deriving (Show)

-- "common" operands are for the set of  arithmetic/memory instructions
-- which all accept a lot (15) of operand types
isCommonOp :: Operand -> Bool
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

-- operand types for CPX and CPY
isCompareOp :: Operand -> Bool
isCompareOp (Imm8 _) = True
isCompareOp (Imm16 _) = True
isCompareOp (Dir _) = True
isCompareOp (Abs _) = True
isCompareOp _ = False

-- operand types for INC, DEC, ASL, LSR, ROL, ROR
isShiftOp :: Operand -> Bool
isShiftOp Accumulator = True
isShiftOp (Dir _) = True
isShiftOp (Abs _) = True
isShiftOp (DirX _) = True
isShiftOp (AbsX _) = True
isShiftOp _ = False

-- operand types for BIT
isBitOp :: Operand -> Bool
isBitOp (Dir _) = True
isBitOp (Abs _) = True
isBitOp (DirX _) = True
isBitOp (AbsX _) = True
isBitOp (Imm8 _) = True
isBitOp (Imm16 _) = True
isBitOp _ = False

-- operand types for TRB and TSB
isTestOp :: Operand -> Bool
isTestOp (Dir _) = True
isTestOp (Abs _) = True
isTestOp _ = False

-- operand types for BCC, BCS, BEQ, BMI, BNE, BPL, BRA, BVC, BVS
isBranchOp :: Operand -> Bool
isBranchOp (Label8 _) = True
isBranchOp _ = False

-- operand types for JMP
isJumpOp :: Operand -> Bool
isJumpOp (Label16 _) = True
isJumpOp (Label24 _) = True
isJumpOp (AbsInd _) = True -- but these can be labels!
isJumpOp (AbsXInd _) = True -- but these can be labels!
isJumpOp (AbsIndLong _) = True -- but these can be labels!
isJumpOp _ = False

-- operand types for JSR
isJsrOp :: Operand -> Bool
isJsrOp (Label16 _) = True
isJsrOp (AbsXInd _) = True -- but these can be labels!
isJsrOp _ = False

-- operand types for LDX
isLoadXOp :: Operand -> Bool
isLoadXOp (Imm8 _) = True
isLoadXOp (Imm16 _) = True
isLoadXOp (Abs _) = True
isLoadXOp (AbsY _) = True
isLoadXOp (Dir _) = True
isLoadXOp (DirY _) = True
isLoadXOp _ = False

-- operand types for LDY
isLoadYOp :: Operand -> Bool
isLoadYOp (Imm8 _) = True
isLoadYOp (Imm16 _) = True
isLoadYOp (Abs _) = True
isLoadYOp (AbsX _) = True
isLoadYOp (Dir _) = True
isLoadYOp (DirX _) = True
isLoadYOp _ = False

-- operand types for STX
isStoreXOp :: Operand -> Bool
isStoreXOp (Abs _) = True
isStoreXOp (Dir _) = True
isStoreXOp (DirY _) = True
isStoreXOp _ = False

-- operand types for STY
isStoreYOp :: Operand -> Bool
isStoreYOp (Abs _) = True
isStoreYOp (Dir _) = True
isStoreYOp (DirX _) = True
isStoreYOp _ = False

-- operand types for STZ
isStoreZOp :: Operand -> Bool
isStoreZOp (Abs _) = True
isStoreZOp (AbsX _) = True
isStoreZOp (Dir _) = True
isStoreZOp (DirX _) = True
isStoreZOp _ = False

isValidOperand :: Instruction -> Bool
isValidOperand (ADC operand) = isCommonOp operand
isValidOperand (SBC operand) = isCommonOp operand
isValidOperand (CMP operand) = isCommonOp operand
isValidOperand (CPX operand) = isCompareOp operand
isValidOperand (CPY operand) = isCompareOp operand
isValidOperand (DEC operand) = isShiftOp operand
isValidOperand (INC operand) = isShiftOp operand
isValidOperand (AND operand) = isCommonOp operand
isValidOperand (EOR operand) = isCommonOp operand
isValidOperand (ORA operand) = isCommonOp operand
isValidOperand (BIT operand) = isBitOp operand
isValidOperand (TRB operand) = isTestOp operand
isValidOperand (TSB operand) = isTestOp operand
isValidOperand (ASL operand) = isShiftOp operand
isValidOperand (LSR operand) = isShiftOp operand
isValidOperand (ROR operand) = isShiftOp operand
isValidOperand (ROL operand) = isShiftOp operand
isValidOperand (BCC operand) = isBranchOp operand
isValidOperand (BCS operand) = isBranchOp operand
isValidOperand (BEQ operand) = isBranchOp operand
isValidOperand (BMI operand) = isBranchOp operand
isValidOperand (BNE operand) = isBranchOp operand
isValidOperand (BPL operand) = isBranchOp operand
isValidOperand (BRA operand) = isBranchOp operand
isValidOperand (BVC operand) = isBranchOp operand
isValidOperand (BRL (Label16 _)) = True -- only valid operand type
isValidOperand (JMP operand) = isJumpOp operand
isValidOperand (JSL (Label24 _)) = True -- only valid operand type
isValidOperand (JSR operand) = isJsrOp operand
isValidOperand (BRK (Imm8 _)) = True -- only valid operand type
isValidOperand (COP (Imm8 _)) = True -- only valid operand type
isValidOperand (REP (Imm8 _)) = True -- only valid operand type
isValidOperand (SEP (Imm8 _)) = True -- only valid operand type
isValidOperand (LDA operand) = isCommonOp operand
isValidOperand (LDX operand) = isLoadXOp operand
isValidOperand (LDY operand) = isLoadYOp operand
isValidOperand (STA (Imm8 _)) = False -- forbid store to immediate
isValidOperand (STA (Imm16 _)) = False -- forbid store to immediate
isValidOperand (STA operand) = isCommonOp operand -- all other common ops okay
isValidOperand (STX operand) = isStoreXOp operand
isValidOperand (STY operand) = isStoreYOp operand
isValidOperand (STZ operand) = isStoreZOp operand
isValidOperand (MVN (Imm8 _) (Imm8 _)) = True -- only valid operand type
isValidOperand (MVP (Imm8 _) (Imm8 _)) = True -- only valid operand type
isValidOperand (WDM (Imm8 _)) = True -- only valid operand type
isValidOperand (PEA (Imm16 _)) = True -- only valid operand type
isValidOperand (PEI (Dir _)) = True -- only valid operand type
isValidOperand (PER (Label16 _)) = True -- only valid operand type
isValidOperand _ = False -- everything else is false
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

For example, we could use the nocash syntax
(https://problemkaputt.de/fullsnes.htm#cpu65xxmicroprocessor)
which turns the various register-to-register transfer instructinos into MOVs:

  Opcode    Flags Clks  Native  Nocash    Bits  Effect
  A8        nz----  2   TAY     MOV Y,A   x     Y=A
  AA        nz----  2   TAX     MOV X,A   x     X=A
  BA        nz----  2   TSX     MOV X,S   x     X=S
  98        nz----  2   TYA     MOV A,Y   m     A=Y
  8A        nz----  2   TXA     MOV A,X   m     A=X
  9A        ------  2   TXS     MOV S,X   e     S=X
  9B        nz----  2   TXY     MOV Y,X   x     Y=X
  BB        nz----  2   TYX     MOV X,Y   x     X=Y
  7B        nz----  2   TDC     MOV A,D   16    A=D
  5B        nz----  2   TCD     MOV D,A   16    D=A
  3B        nz----  2   TSC     MOV A,SP  16    A=SP
  1B        ------  2   TCS     MOV SP,A  e?    SP=A

Is this worth it?

-}
