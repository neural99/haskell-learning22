{- 

Little man 8-bit computer


HLT - halt the emulator and return register value
ADD - add value from memory to register
SUB - subtract memory value from register value
MUL - multiply memory value with register value
REM - remainder
DIV - integer division
SHL - shift left
SHR - shift right
LDA - load value from memory to register
STA - store value from register to memory
BRA - branch
BRZ - branch if zero
BRP - branch if positive
INP - read character from user and store in register
OUT - print character from register 

-}

import Data.Word
import Data.Bits

type Byte = Word8
type Adress = Int
type Memory = [Word8]

data CPU = CPU { register :: Word8
	       , memory :: Memory 
	       , isRunning :: Bool
	       } deriving (Show)

updateMemory :: Memory -> Adress -> Word8 -> Memory
updateMemory mem adress val = take adress mem ++ val : drop (adress + 1) mem

defaultCPU :: CPU
defaultCPU = CPU { register = 0, memory = take 0xffff (repeat 0), isRunning = True }

hlt :: CPU -> CPU
hlt cpu = cpu { isRunning = False }

add :: CPU -> Adress -> CPU
add cpu adress = cpu { register = (register cpu) +  (memory cpu !! adress) }

sub :: CPU -> Adress -> CPU
sub cpu adress = cpu { register = (register cpu) -  (memory cpu !! adress) }

mul :: CPU -> Adress -> CPU
mul cpu adress = cpu { register = (register cpu) * (memory cpu !! adress) }

rem :: CPU -> Adress -> Adress -> CPU
rem cpu adress1 adress2 = let operand1 = (memory cpu !! adress1) 
			      operand2 = (memory cpu !! adress2) 
			      in cpu { register = operand1 `Prelude.rem` operand2 }

div :: CPU -> Adress -> Adress -> CPU
div cpu adress1 adress2 = let operand1 = (memory cpu !! adress1) 
			      operand2 = (memory cpu !! adress2) 
			      in cpu { register = operand1 `Prelude.div` operand2 }

shl :: CPU -> CPU
shl cpu = cpu { register = shift (register cpu) 1 }

shr :: CPU -> CPU
shr cpu = cpu { register = shift (register cpu) (-1) }

lda :: CPU -> Adress -> CPU
lda cpu adress = cpu { register = (memory cpu) !! adress }


sta :: CPU -> Adress -> CPU
sta cpu adress = cpu { memory = updateMemory (memory cpu) adress (register cpu)  }
