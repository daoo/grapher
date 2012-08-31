module ForceGraph.Defaults where

import Data.Vector2
import ForceGraph.Ball
import ForceGraph.Rectangle
import ForceGraph.World

defaultWorld :: World
defaultWorld = World
  { worldBoundary = Rectangle 500 500
  , worldBalls =
    [ Ball { pos = Vector2 250 250, vel = zero, radius = 20, mass = 10, charge = 100 }
    , Ball { pos = Vector2 150 200, vel = zero, radius = 10, mass = 1, charge = 100 }
    , Ball { pos = Vector2 200 250, vel = zero, radius = 10, mass = 1, charge = 100 }
    , Ball { pos = Vector2 100 540, vel = zero, radius = 10, mass = 1, charge = 100 }
    , Ball { pos = Vector2 500 540, vel = zero, radius = 10, mass = 1, charge = 100 }
    , Ball { pos = Vector2 510 570, vel = zero, radius = 10, mass = 1, charge = 100 }
    , Ball { pos = Vector2 520 540, vel = zero, radius = 10, mass = 1, charge = 100 }
    , Ball { pos = Vector2 530 530, vel = zero, radius = 10, mass = 1, charge = 100 }
    , Ball { pos = Vector2 540 520, vel = zero, radius = 10, mass = 1, charge = 100 }
    ]
  , worldLinks =
    [ (0, 1)
    , (0, 2)
    , (0, 7)
    , (0, 8)
    , (1, 3)
    , (3, 4)
    , (4, 5)
    , (5, 6)
    ]
  }
