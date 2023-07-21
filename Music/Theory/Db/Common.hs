-- | Database as [[(key,value)]]
module Music.Theory.Db.Common where

import Data.List {- base -}
import Data.Maybe {- base -}
import Safe {- safe -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Maybe as Maybe {- hmt-base -}

-- * Type

type Entry k v = (k,v)
type Record k v = [Entry k v]
type Db k v = [Record k v]

type TextKey = String
type TextValue = String
type TextEntry = Entry TextKey TextValue
type TextRecord = Record TextKey TextValue
type TextDb = Db TextKey TextValue

-- * Record

-- | The sequence of keys at 'Record'.
record_key_seq :: Record k v -> [k]
record_key_seq = map fst

-- | 'True' if 'Key' is present in 'Entity'.
record_has_key :: Eq k => k -> Record k v -> Bool
record_has_key k = elem k . record_key_seq

-- | 'List.histogram' of 'record_key_seq'.
record_key_histogram :: Ord k => Record k v -> [(k,Int)]
record_key_histogram = List.histogram . record_key_seq

-- | Duplicate keys predicate.
record_has_duplicate_keys :: Ord k => Record k v -> Bool
record_has_duplicate_keys = any ((> 0) . snd) . record_key_histogram

-- | Find all associations for key using given equality function.
record_lookup_by :: (k -> k -> Bool) -> k -> Record k v -> [v]
record_lookup_by f k = map snd . filter (f k . fst)

-- | 'record_lookup_by' of '=='.
record_lookup :: Eq k => k -> Record k v -> [v]
record_lookup = record_lookup_by (==)

-- | /n/th element of 'record_lookup'.
record_lookup_at :: Eq k => (k,Int) -> Record k v -> Maybe v
record_lookup_at (c,n) = flip atMay n . record_lookup c

-- | Variant of 'record_lookup' requiring a unique key.  'Nothing' indicates
-- there is no entry, it is an 'error' if duplicate keys are present.
record_lookup_uniq :: Eq k => k -> Record k v -> Maybe v
record_lookup_uniq k r =
    case record_lookup k r of
      [] -> Nothing
      [v] -> Just v
      _ -> error "record_lookup_uniq: non uniq"

-- | 'True' if key exists and is unique.
record_has_key_uniq :: Eq k => k -> Record k v -> Bool
record_has_key_uniq k = isJust . record_lookup_uniq k

-- | Error variant.
record_lookup_uniq_err :: Eq k => k -> Record k v -> v
record_lookup_uniq_err k = Maybe.from_just "record_lookup_uniq: none" . record_lookup_uniq k

-- | Default value variant.
record_lookup_uniq_def :: Eq k => v -> k -> Record k v -> v
record_lookup_uniq_def v k = fromMaybe v . record_lookup_uniq k

-- | Remove all associations for key using given equality function.
record_delete_by :: (k -> k -> Bool) -> k -> Record k v -> Record k v
record_delete_by f k = filter (not . f k . fst)

-- | 'record_delete_by' of '=='.
record_delete :: Eq k => k -> Record k v -> Record k v
record_delete = record_delete_by (==)

-- * Db

-- | Preserves order of occurence.
db_key_set :: Ord k => Db k v -> [k]
db_key_set = nub . map fst . concat

db_lookup_by :: (k -> k -> Bool) -> (v -> v -> Bool) -> k -> v -> Db k v -> [Record k v]
db_lookup_by k_cmp v_cmp k v =
    let f = any (v_cmp v) . record_lookup_by k_cmp k
    in filter f

db_lookup :: (Eq k,Eq v) => k -> v -> Db k v -> [Record k v]
db_lookup = db_lookup_by (==) (==)

db_has_duplicate_keys :: Ord k => Db k v -> Bool
db_has_duplicate_keys = any record_has_duplicate_keys

db_key_histogram :: Ord k => Db k v -> [(k,Int)]
db_key_histogram db =
    let h = concatMap record_key_histogram db
        f k = (k,maximum (record_lookup k h))
    in map f (db_key_set db)

db_to_table :: Ord k => (Maybe v -> e) -> Db k v -> ([k],[[e]])
db_to_table f db =
    let kh = db_key_histogram db
        hdr = concatMap (\(k,n) -> replicate n k) kh
        ix = concatMap (\(k,n) -> zip (repeat k) [0 .. n - 1]) kh
    in (hdr,map (\r -> map (\i -> f (record_lookup_at i r)) ix) db)

-- * Collating duplicate keys.

record_collate_from :: Eq k => (k,[v]) -> Record k v -> Record k [v]
record_collate_from (k,v) r =
    case r of
      [] -> [(k,reverse v)]
      (k',v'):r' ->
          if k == k'
          then record_collate_from (k,v' : v) r'
          else (k,reverse v) : record_collate_from (k',[v']) r'

-- | Collate adjacent entries of existing sequence with equal key.
record_collate :: Eq k => Record k v -> Record k [v]
record_collate r =
    case r of
      [] -> error "record_collate: nil"
      (k,v):r' -> record_collate_from (k,[v]) r'

record_uncollate :: Record k [v] -> Record k v
record_uncollate = concatMap (\(k,v) -> zip (repeat k) v)
