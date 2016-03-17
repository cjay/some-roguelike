module Behave where

type Pos = (Int, Int)
data Smell = Sweet | Pungent
newtype EntId = EntId Int

data Query r where
    See :: Query Pos
    Weight :: Query Int
    Height :: Query Int
    Smell :: Query Smell

data Action = DealDamage Int | Heat Int

data Component =
    Physical {
      pos :: Pos,
      weight :: Int,
      height :: Int
    } |
    Chemical {
      flashPoint :: Int
    } |
    Appearance {
      visibility :: Bool,
      offset :: (Int, Int)
    } |
    Render {
      img :: String
    }

-- derp
behave :: Action -> Component -> Int
behave (Heat temp) (Chemical fp) = 0
