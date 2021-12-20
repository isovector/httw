module Languages.Information where

import Diagram
import Diagrams.Prelude hiding (E)
import Diagrams.Backend.Rasterific

data Division = N | W | E | S
  deriving stock (Eq, Ord, Show, Enum, Bounded)

clear :: Diagram B
clear = withEnvelope ((rect 1 1 # center :: Diagram B) ^. envelope) mempty

instance ToDiagram [Division] where
  toDiagram d = followDivision d # center `atop` rect 1 1 # center

followDivision :: [Division] -> Diagram B
followDivision [] = rect 1 1 # fc black
followDivision (N : dis) = (followDivision dis # alignB # alignL === clear # alignL) # scaleY 0.5
followDivision (W : dis) = (followDivision dis # alignR # alignB ||| clear # alignB) # scaleX 0.5
followDivision (E : dis) = (clear # alignB ||| followDivision dis # alignL # alignB) # scaleX 0.5
followDivision (S : dis) = (clear # alignL === followDivision dis # alignT # alignL) # scaleY 0.5

