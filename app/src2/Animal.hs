module Animal where

import Entity

type Species = "Fox" | "Rabbit"

data Animal = Animal { species  :: Species,
                       sex      :: Bool,
                       hunger   :: Double,
                       thirst   :: Double,
                       urge     :: Double,
                       fovDist  :: Int,
                       speed    :: Int,
                       ttd      :: Int
                     }


instance Entity Animal where
  
