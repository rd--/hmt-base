-- | Map functions.
module Music.Theory.Map where

import Data.Maybe {- base -}

import qualified Data.Map as Map {- containers -}

-- | Erroring 'Map.lookup'.
map_lookup_err :: Ord k => k -> Map.Map k c -> c
map_lookup_err k = fromMaybe (error "Map.lookup") . Map.lookup k

-- | 'flip' of 'Map.lookup'.
map_ix :: Ord k => Map.Map k c -> k -> Maybe c
map_ix = flip Map.lookup

-- | 'flip' of 'map_lookup_err'.
map_ix_err :: Ord k => Map.Map k c -> k -> c
map_ix_err = flip map_lookup_err
