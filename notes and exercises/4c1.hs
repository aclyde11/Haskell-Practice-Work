-- Austin Clyde
-- Class 10

-- Record Types

-- Record Types allow us to enforce certain type orderings
-- Without uncessarry wraping

data Person
  = Student { firstName :: String
            , lastName  :: String
            , age       :: Int
            , year      :: Int
            }
  | Teacher { firstName :: String
            , lastName  :: String
            }
    deriving (Eq, Show)

-- Records are only expressions inside of fields. They are not
-- Available outside of the consutrctor statement.
-- They also cannot be partially applied.


-- This is not needed as Haskell records auto generate
-- functions by the field names of the record
surname (Student _ s _ _) = s


-- New Types

-- Suppose that you actually wanted to wrap types like
-- data Age = AgeInt Int
-- So it acts like a type for complining
-- But is reduced when complied
newtype Age = AgeInt Int
  deriving (Show)


-- List Comprehensions
cartesianProduct as bs = 
  concat $ map (\a -> map (\b -> (a, b)) bs) as

cartProdBetter as bs = [ (a, b) | a <- as, b <-bs]
