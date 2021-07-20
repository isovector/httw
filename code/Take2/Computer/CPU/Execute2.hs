module Take2.Computer.CPU.Execute2 where

import Prelude hiding ((.), id, sum)
import Take2.Computer.Register
import Take2.Computer.Instruction
import Take2.Computer.Poly
import Circuitry.Machinery
import Take2.Computer.Simple (eq)
import Take2.Computer.CPU.Utils (getReg1)



aluBinaryOp2
    :: Circuit ( (Register, (Register, Register))
               , (Registers PC SP W, W)
               )
               (Registers PC SP W)
aluBinaryOp2
    = (snd' >>> snd') *** swap
  >>> reassoc
  >>> setReg

aluUnaryOp2
    :: Circuit ( (Register, Register)
               , (Registers PC SP W, W)
               )
               (Registers PC SP W)
aluUnaryOp2
    = snd' *** swap
  >>> reassoc
  >>> setReg


execute2
    :: Circuit (Instruction, (Registers PC SP W, W)) (Registers PC SP W)
execute2
    = elim
    $ foldElim
    $ #_INop :-> snd' >>> fst'
  :+| #_IAdd :-> aluBinaryOp2
  :+| #_IAddI :-> (snd' >>> snd')
              *** swap
              >>> reassoc
              >>> setReg
  :+| #_IAnd :-> aluBinaryOp2
  :+| #_IOr  :-> aluBinaryOp2
  :+| #_IXOr :-> aluBinaryOp2
  :+| #_INot :-> aluUnaryOp2
  :+| #_IMove :-> second' fst' >>> move
  :+| #_ILoadHi :-> loadHi
  :+| #_ILoadLo :-> loadLo
  :+| #_IShiftL :-> aluUnaryOp2
  :+| #_IShiftR :-> aluUnaryOp2
  :+| #_IAShiftR :-> aluUnaryOp2
  :+| #_IJump :-> snd'
              >>> swap
              >>> replace #reg_PC
  :+| #_IBranchZ :-> branch2
  :+| End

move :: Circuit ((Register, Register), Registers PC SP W) (Registers PC SP W)
move
    = swap
  >>> distribP
  >>> getReg
  *** swap
  >>> reassoc
  >>> first' swap
  >>> setReg


loadLo
    :: Circuit ((Register, Word8), (Registers PC SP W, W)) (Registers PC SP W)
loadLo = loadLoOrHigh id


loadHi
    :: Circuit ((Register, Word8), (Registers PC SP W, W)) (Registers PC SP W)
loadHi = loadLoOrHigh swap


loadLoOrHigh
    :: (forall a. Embed a => Circuit (a, a) (a, a))
    -> Circuit ((Register, Word8), (Registers PC SP W, W)) (Registers PC SP W)
loadLoOrHigh f
    = swap *** (fst' >>> copy)
  >>> reassoc
  >>> first'
      ( reassoc'
    >>> second'
        ( first' copy
      >>> reassoc'
      >>> second'
          ( swap
        >>> getReg
        >>> serial
        >>> separate @(SizeOf HalfW)
        >>> f
        >>> snd'
          )
        )
      >>> second' swap
      >>> reassoc
      >>> first' (first' serial >>> f >>> serial >>> unsafeParse @W)
      >>> swap
      )
    >>> setReg



branch2 :: Circuit ((Register, HalfW), (Registers PC SP W, W))
                   (Registers PC SP W)
branch2 = reassoc
      >>> first'
          ( getReg1
        >>> first' (intro 0 >>> eq)
        >>> second' snd'
          )
      >>> reassoc'
      >>> ifC (swap >>> replace #reg_PC) fst'

