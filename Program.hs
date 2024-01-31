import Data.Char (ord)
import Data.Word

{-

Important references:
https://archive.org/details/SNESDevManual/book1/page/n27/mode/2up
https://sneslab.net/wiki/SNES_ROM_Header
https://problemkaputt.de/fullsnes.htm#snescartridgeromheader

There were three formats for ROM headers. So far this is only the original
format.

A note about the ROM header: it is entirely unnecessary. Games will run
perfectly fine on original hardware if the information is not included.
However, emulators may be unhappy if the header is incorrect.

-}
type MakerCode = (Word8, Word8)

type GameCode = (Word8, Word8, Word8, Word8)

type CartridgeTitle = [Word8] -- must be 21 bytes, pad extra chars with spaces

{-
Note: the setting in the ROM header does not affect the speed of the ROM; it
merely indicates the use of enhanced memory to allow quicker reads from ROM.
The program must still set a hardware register in order to enable fast ROM
accesses.

FastROM speeds up data access times in the following regions:
  $80-$BF:8000-FFFF
  $C0:0000-$FF:FFFF
The speedup is achieved by making the CPU not wait as long for data to be
received from the cartridge. This requires a cartridge powerful enough to meet
the needs of faster acccesses. It actually turns out almost every cartridge was
capable of doing this.
-}
data ROMSpeed
  = SlowROM
  | FastROM

{-
There are some other modes, not all documented here

LoROM is the standard in most SNES games. The main benefit of HiROM is the
ability to lay out memory in contiguous 64K blocks as opposed to 32K blocks.
However, it loses having hardware registers and WRAM mirrored in the same bank
as ROM contents.

ExHiROM allows for the greatest use of the address space for the ROM, at nearly
8 MB. But not all ROM in that region can take advantage of Fast ROM.

ROM Mappers allow for much greater use of space but require more more effort
to understand :(
-}
data MappingModes
  = LoROM
  | HiROM
  | SuperMMC -- ROM mapper, could be useful for very large games
  | ExHiROM

-- This is not a complete list of Cartridge types. There are like a bunch
-- because the SNES had many console enhancement chips. (Should we include them
-- as standard? Provide a facility to use them? Automatically determine if one
-- is needed?)
data CartridgeType
  = ROMOnly
  | ROMPlusRAM
  | ROMPlusRAMAndBattery

data Region
  = Japan
  | NorthAmerica
  | Europe
  | Sweden
  | Finland
  | Denmark
  | France
  | Netherlands
  | Spain
  | Germany
  | Italy
  | China
  | Indonesia
  | SouthKorea
  | Common
  | Canada
  | Brazil
  | Australia

{-
Eventually here, we will provide a facility where the user specifies the header
of their ROM and we give back the corresponding bit patterns.
-}
createHeader = undefined

{-

How will we lay out the code in the ROM? The key to good optimization is a good
layout of code and data in the ROM. Who is doing that? Certainly not the 65816
utilities; the compiler needs to do that.

-}
