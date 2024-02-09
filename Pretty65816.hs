import Prettyprinter
import Prettyprinter.Render.Text
import System.IO

import Instructions

instance Pretty Instruction where
  pretty (ADC o) = indent 4 (group (pretty "ADC" <> pretty o))
  pretty (SBC o) = indent 4 (group (pretty "SBC" <> pretty o))
  pretty (CMP o) = indent 4 (group (pretty "CMP" <> pretty o))
  pretty (CPX o) = indent 4 (group (pretty "CPX" <> pretty o))
  pretty (CPY o) = indent 4 (group (pretty "CPY" <> pretty o))
  pretty (DEC o) = indent 4 (group (pretty "DEC" <> pretty o))
  pretty (DEX) = indent 4 (pretty "DEX")
  pretty (DEY) = pretty "DEY"
  pretty (INC o) = indent 4 (group (pretty "INC" <> pretty o))
  pretty (INX) = indent 4 (pretty "INX")
  pretty (INY) = indent 4 (pretty "INY")
  pretty (AND o) = indent 4 (group (pretty "AND" <> pretty o))
  pretty (EOR o) = indent 4 (group (pretty "EOR" <> pretty o))
  pretty (ORA o) = indent 4 (group (pretty "ORA" <> pretty o))
  pretty (BIT o) = indent 4 (group (pretty "BIT" <> pretty o))
  pretty (TRB o) = indent 4 (group (pretty "TRB" <> pretty o))
  pretty (TSB o) = indent 4 (group (pretty "TSB" <> pretty o))
  pretty (ASL o) = indent 4 (group (pretty "ASL" <> pretty o))
  pretty (LSR o) = indent 4 (group (pretty "LSR" <> pretty o))
  pretty (ROL o) = indent 4 (group (pretty "ROL" <> pretty o))
  pretty (ROR o) = indent 4 (group (pretty "ROR" <> pretty o))
  pretty (BCC o) = indent 4 (group (pretty "BCC" <> pretty o))
  pretty (BCS o) = indent 4 (group (pretty "BCS" <> pretty o))
  pretty (BEQ o) = indent 4 (group (pretty "BEQ" <> pretty o))
  pretty (BMI o) = indent 4 (group (pretty "BMI" <> pretty o))
  pretty (BNE o) = indent 4 (group (pretty "BNE" <> pretty o))
  pretty (BPL o) = indent 4 (group (pretty "BPL" <> pretty o))
  pretty (BRA o) = indent 4 (group (pretty "BRA" <> pretty o))
  pretty (BVC o) = indent 4 (group (pretty "BVC" <> pretty o))
  pretty (BVS o) = indent 4 (group (pretty "BVS" <> pretty o))
  pretty (BRL o) = indent 4 (group (pretty "BRL" <> pretty o))
  pretty (JMP o) = indent 4 (group (pretty "JMP" <> pretty o))
  pretty (JSL o) = indent 4 (group (pretty "JSL" <> pretty o))
  pretty (JSR o) = indent 4 (group (pretty "JSR" <> pretty o))
  pretty (RTL) = indent 4 (pretty "RTL")
  pretty (RTS) = indent 4 (pretty "RTS")
  pretty (BRK o) = indent 4 (group (pretty "BRK" <> pretty o))
  pretty (COP o) = indent 4 (group (pretty "COP" <> pretty o))
  pretty (RTI) = indent 4 (pretty "RTI")
  pretty (CLC) = indent 4 (pretty "CLC")
  pretty (CLD) = indent 4 (pretty "CLD")
  pretty (CLI) = indent 4 (pretty "CLI")
  pretty (CLV) = indent 4 (pretty "CLV")
  pretty (SEC) = indent 4 (pretty "SEC")
  pretty (SED) = indent 4 (pretty "SED")
  pretty (SEI) = indent 4 (pretty "SEI")
  pretty (REP o) = indent 4 (group (pretty "REP" <> pretty o))
  pretty (SEP o) = indent 4 (group (pretty "SEP" <> pretty o))
  pretty (LDA o) = indent 4 (group (pretty "LDA" <> pretty o))
  pretty (LDX o) = indent 4 (group (pretty "LDX" <> pretty o))
  pretty (LDY o) = indent 4 (group (pretty "LDY" <> pretty o))
  pretty (STA o) = indent 4 (group (pretty "STA" <> pretty o))
  pretty (STX o) = indent 4 (group (pretty "STX" <> pretty o))
  pretty (STY o) = indent 4 (group (pretty "STY" <> pretty o))
  pretty (STZ o) = indent 4 (group (pretty "STZ" <> pretty o))
  pretty (MVN (Imm8 i1) (Imm8 i2)) =
    indent 4 (group (pretty "MVN" <> pretty i1 <> pretty "," <> pretty i2))
  pretty (MVP (Imm8 i1) (Imm8 i2)) =
    indent 4 (group (pretty "MVP" <> pretty i1 <> pretty "," <> pretty i2))
  pretty (NOP) = indent 4 (pretty "NOP")
  pretty (WDM o) = indent 4 (group (pretty "WDM" <> pretty o))
  pretty (PEA o) = indent 4 (group (pretty "PEA" <> pretty o))
  pretty (PEI o) = indent 4 (group (pretty "PEI" <> pretty o))
  pretty (PER o) = indent 4 (group (pretty "PER" <> pretty o))
  pretty (PHA) = indent 4 (pretty "PHA")
  pretty (PHX) = indent 4 (pretty "PHX")
  pretty (PHY) = indent 4 (pretty "PHY")
  pretty (PLA) = indent 4 (pretty "PLA")
  pretty (PLX) = indent 4 (pretty "PLX")
  pretty (PLY) = indent 4 (pretty "PLY")
  pretty (PHB) = indent 4 (pretty "PHB")
  pretty (PHD) = indent 4 (pretty "PHD")
  pretty (PHK) = indent 4 (pretty "PHK")
  pretty (PHP) = indent 4 (pretty "PHP")
  pretty (PLB) = indent 4 (pretty "PLB")
  pretty (PLD) = indent 4 (pretty "PLD")
  pretty (PLP) = indent 4 (pretty "PLP")
  pretty (STP) = indent 4 (pretty "STP")
  pretty (WAI) = indent 4 (pretty "WAI")
  pretty (TAX) = indent 4 (pretty "TAX")
  pretty (TAY) = indent 4 (pretty "TAY")
  pretty (TSX) = indent 4 (pretty "TSX")
  pretty (TXA) = indent 4 (pretty "TXA")
  pretty (TXS) = indent 4 (pretty "TXS")
  pretty (TXY) = indent 4 (pretty "TXY")
  pretty (TYA) = indent 4 (pretty "TYA")
  pretty (TYX) = indent 4 (pretty "TYX")
  pretty (TCD) = indent 4 (pretty "TCD")
  pretty (TCS) = indent 4 (pretty "TCS")
  pretty (TDC) = indent 4 (pretty "TDC")
  pretty (TSC) = indent 4 (pretty "TSC")
  pretty (XBA) = indent 4 (pretty "XBA")
  pretty (XCE) = indent 4 (pretty "XCE")
  pretty (Label s) = group (pretty s <> pretty ":")

instance Pretty Operand where
  pretty (Imm8 w) = pretty ".B" <+> pretty "#" <> pretty w
  pretty (Imm16 w) = pretty ".W" <+> pretty "#" <> pretty w
  pretty (Abs w) = pretty ".W" <+> pretty w
  pretty (AbsX w) = pretty ".W" <+> pretty w <> pretty ",X"
  pretty (AbsY w) = pretty ".W" <+> pretty w <> pretty ",Y"
  pretty (Dir w) = pretty ".B" <+> pretty w
  pretty (DirX w) = pretty ".B" <+> pretty w <> pretty ",X"
  pretty (DirY w) = pretty ".B" <+> pretty w <> pretty ",Y"
  pretty (Accumulator) = pretty "A"
  pretty (DirInd w) = pretty ".B" <+> parens (pretty w)
  pretty (Long w) = pretty ".L" <+> pretty w
  pretty (LongX w) = pretty ".L" <+> pretty w <> pretty ",X"
  pretty (DirIndLong w) = pretty ".B" <+> brackets (pretty w)
  pretty (DirIndY w) = pretty ".B" <+> parens (pretty w) <> pretty ",Y"
  pretty (DirXInd w) = pretty ".B" <+> parens (pretty w <> pretty ",X")
  pretty (AbsInd w) = pretty ".W" <+> parens (pretty w)
  pretty (AbsIndLong w) = pretty ".W" <+> brackets (pretty w)
  pretty (AbsXInd w) = pretty ".W" <+> parens (pretty w <> pretty ",X")
  pretty (DirIndLongY w) = pretty ".B" <+> brackets (pretty w) <> pretty ",Y"
  pretty (Stack w) = pretty ".B" <+> pretty w <> pretty ",S"
  pretty (StackIndY w) =
    pretty ".B" <+> parens (pretty w <> pretty ",S") <> pretty ",Y"
  pretty (Label8 s) = pretty ".B" <+> pretty s
  pretty (Label16 s) = pretty ".W" <+> pretty s
  pretty (Label24 s) = pretty ".L" <+> pretty s

putDocCompact :: Doc a -> IO ()
putDocCompact = renderIO System.IO.stdout . layoutCompact
