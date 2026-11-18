
{- HLINT ignore "Use newtype instead of data" -}

-- | Домашка 8. Аппликативные функторы: обязательные задачи.
module Level1 where

import Console (MyIO, readInt)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.Read (readMaybe)


-- 1.1. Разбор записи об игроке.

data Player = Player
  { playerName :: String
  , playerScore :: Integer
  }
  deriving (Eq, Show)

parseName :: String -> Maybe String
parseName name =
  if null name then Nothing else Just name

parseScore :: String -> Maybe Integer
parseScore = readMaybe

parsePlayer :: String -> Maybe Player
parsePlayer str =
  case break (== ':') str of
    (name, ':' : score) ->
      Player <$> parseName name <*> parseScore score
    _ -> Nothing


-- 1.2. Точка на плоскости.

data Point a = Point a a
  deriving (Eq, Show, Functor)

instance Applicative Point where
  pure x = Point x x

  Point f g <*> Point x y = Point (f x) (g y)

stretchPoint :: Num a => a -> Point a -> Point a
stretchPoint k p = (k *) <$> p

sumPoints :: Num a => Point a -> Point a -> Point a -> Point a
sumPoints p1 p2 p3 =
  (\x y z -> x + y + z) <$> p1 <*> p2 <*> p3


-- 1.3. Список вычислений.

newtype UserId = UserId Int
  deriving (Eq, Ord, Show)

data User = User { subscribed :: Bool }
  deriving (Eq, Show)

type Registry = Map UserId User

allResults :: Applicative f => [f a] -> f [a]
allResults =
  foldr (\x xs -> (:) <$> x <*> xs) (pure [])

check :: Registry -> [UserId] -> Maybe Bool
check registry ids =
  and <$> allResults (map getSubscription ids)
  where
    getSubscription uid =
      subscribed <$> Map.lookup uid registry


-- 1.4. Чтение нескольких чисел.

readInts :: Int -> MyIO [Int]
readInts n
  | n <= 0 = pure []
  | otherwise = (:) <$> readInt <*> readInts (n - 1)
