module Take2.Computer.CPU where

import Prelude hiding ((.), id, sum)
import Take2.Computer.ALU
import Take2.Computer.Bus
import Take2.Computer.Register
import Take2.Computer.CPU.Execute1
import Take2.Computer.CPU.Execute2
import Take2.Computer.Instruction
import Take2.Computer.Poly
import Circuitry.Machinery



data Step
  = Fetch
  | Decode  W
  | Execute Instruction
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Embed, Reify)


cpu
    :: Vec (2 ^ N) W
    -> Circuit () (Registers PC SP W)
cpu rom
    = registers
    $ fixC Fetch
    $ first' snd'
  >>> swap
  >>> first' copy
  >>> second' copy
  >>> reassoc'
  >>> second' reassoc
  >>> second' ( first'
              $ cpuImpl1
            >>> cpuBus rom
              )
  >>> second' swap
  >>> cpuImpl2
  >>> swap
  >>> first' copy


cpuImpl1
    :: Circuit (Step, Registers PC SP W)
               (BusCommand N W)
cpuImpl1 =
  elim $ foldElim
       $ #_Fetch :~>
          fetch
     :+| #_Decode :->
          snd' >>> incPC
     :+| #_Execute :->
          first' (traceC "execute") >>> execute1
     :+| End


incPC
    :: Circuit (Registers PC SP W) (BusCommand N W)
incPC = proj #reg_PC
    >>> intro @W 1
    >>> inj @(AluCommand W) #_AluAdd
    >>> inj #_BusAlu


cpuBus
    :: Vec (2 ^ N) W
    -> Circuit (BusCommand N W) W
cpuBus rom  = bus rom >>> unsafeParse


cpuImpl2
    :: Circuit (Step, (Registers PC SP W, W)) (Step, Registers PC SP W)
cpuImpl2 =
  elim $ foldElim
       $ #_Fetch
            :~> swap
            >>> first' (inj #_Decode)
     :+| #_Decode
            :-> (decodeInstr >>> inj #_Execute)
            *** (swap >>> replace #reg_PC)
     :+| #_Execute
            :-> execute2
            >>> intro Fetch
            >>> swap
     :+| End


decodeInstr :: Circuit W Instruction
decodeInstr = unsafeReinterpret


fetch
    :: Circuit (Registers PC SP W) (BusCommand N W)
fetch = proj #reg_PC
    >>> serial
    >>> separate @N
    >>> fst'
    >>> unsafeReinterpret @_ @(Addr N)
    >>> inj #_BusROM

