module Take2.Computer.CPU.Execute1 where

import Prelude hiding ((.), id, sum)
import Take2.Computer.ALU
import Take2.Computer.Bus
import Take2.Computer.Register
import Take2.Computer.Instruction
import Take2.Computer.Poly
import Take2.Computer.CPU.Utils
import Circuitry.Machinery
import Take2.Computer.Simple (ext, sext)


aluBinaryOp1
    :: forall name
     . Inj (AluCommand W) (W, W) name
    => InjName name
    -> Circuit ((Register, (Register, Register)), Registers PC SP W)
               (BusCommand N W)
aluBinaryOp1 n
       = first' (second' fst')
     >>> swap
     >>> distribP
     >>> both getReg
     >>> inj @(AluCommand W)   @(W, W)         n
     >>> inj @(BusCommand N W) @(AluCommand W) #_BusAlu

aluUnaryOp1
    :: forall name
     . Inj (AluCommand W) (W) name
    => InjName name
    -> Circuit ((Register, Register), Registers PC SP W)
               (BusCommand N W)
aluUnaryOp1 n
       = first' fst'
     >>> swap
     >>> getReg
     >>> inj @(AluCommand W)   @W              n
     >>> inj @(BusCommand N W) @(AluCommand W) #_BusAlu

execute1 :: Circuit (Instruction, Registers PC SP W) (BusCommand N W)
execute1
    = elim
    $ foldElim
    $ #_INop :-> todo
  :+| #_IAdd :-> aluBinaryOp1 #_AluAdd
  :+| #_IAddI :-> addI
  :+| #_IAnd :-> aluBinaryOp1 #_AluAnd
  :+| #_IOr  :-> aluBinaryOp1 #_AluOr
  :+| #_IXOr :-> aluBinaryOp1 #_AluXor
  :+| #_INot :-> aluUnaryOp1 #_AluNot
  :+| #_IMove :-> todo    -- not actually todo
  :+| #_ILoadHi :-> todo  -- not actually todo
  :+| #_ILoadLo :-> todo  -- not actually todo
  :+| #_IShiftL :-> aluUnaryOp1 #_AluShiftL
  :+| #_IShiftR :-> aluUnaryOp1 #_AluShiftR
  :+| #_IAShiftR :-> aluUnaryOp1 #_AluAShiftR
  :+| #_IJump :-> first' swap
              >>> reassoc'
              >>> (serial >>> ext >>> unsafeParse @W)
              *** (swap >>> getReg)
              >>> inj @(AluCommand W) #_AluAdd
              >>> inj #_BusAlu
  :+| #_IBranchZ :-> branch1
  :+| End


addI :: Circuit ((Register, (Word4, Register)), Registers PC SP W) (BusCommand N W)
addI = getReg1
   >>> reassoc
   >>> first' (second' (fst' >>> serial >>> sext @(SizeOf W) >>> unsafeParse @W))
   >>> fst'
   >>> inj @(AluCommand W) #_AluAdd
   >>> inj #_BusAlu


branch1 :: Circuit ((Register, HalfW), Registers PC SP W)
                   (BusCommand N W)
branch1 = first' snd'
      >>> second' (proj #reg_PC)
      >>> first' (serial >>> sext >>> unsafeParse @W)
      >>> inj @(AluCommand W) #_AluAdd
      >>> inj #_BusAlu

