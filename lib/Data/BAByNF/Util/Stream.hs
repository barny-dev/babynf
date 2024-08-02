{-# LANGUAGE LambdaCase #-}
module Data.BAByNF.Util.Stream where
import Data.Bifunctor qualified as Bifunctor
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)
import Prelude hiding (take, drop, takeWhile, dropWhile)
import Data.Kind (Type)

newtype Stream e a = Stream
    { runStream :: [e] -> ([e], a)
    }

runStream_ :: Stream e a -> [e] -> a
runStream_ stream = snd . runStream stream
instance Functor (Stream e) where
  fmap func stream = Stream { runStream =  Bifunctor.second func . runStream stream}
instance Applicative (Stream e) where
  pure a = Stream { runStream = (, a) }
  liftA2 func s1 s2 = Stream { runStream = (\(es1, a) -> Bifunctor.second (func a) . runStream s2 $ es1) . runStream s1 }
instance Monad (Stream e) where
  (>>=) s sf = Stream { runStream = (\(es, a) -> runStream (sf a) es ) . runStream s }


hasNext :: Stream e Bool
hasNext = Stream { runStream = \es -> case es of [] -> (es, False); _ -> (es, True) }
take :: Stream e (Maybe e)
take = Stream { runStream = \es -> case es of [] -> (es, Nothing); x:xs -> (xs, Just x)}
drop :: Stream e ()
drop = Stream { runStream = \es -> case es of [] -> (es, ()); _:xs -> (xs, ())}
peek :: Stream e (Maybe e)
peek = Stream { runStream = \es -> case es of [] -> (es, Nothing); x:_ -> (es, Just x)}
takeIf :: (e -> Bool) -> Stream e (Maybe e)
takeIf cond = do
    opt <- peek
    case opt of
        Nothing -> return Nothing
        Just x -> if cond x then drop >> return (Just x) else return Nothing
dropIf :: (e -> Bool) -> Stream e ()
dropIf cond = do
    opt <- peek
    case opt of
        Nothing -> return ()
        Just x -> when (cond x) drop
takeWhile :: (e -> Bool) -> Stream e [e]
takeWhile cond = do
    opt <- takeIf cond
    case opt of
        Nothing -> return []
        Just x -> do
            xs <- takeWhile cond
            return (x:xs)
dropWhile :: (e -> Bool) -> Stream e ()
dropWhile cond = do
    opt <- peek
    case opt of
        Nothing -> return ()
        Just x -> when (cond x) $ drop >> dropWhile cond

find :: (e -> Bool) -> Stream e (Maybe e)
find cond = dropWhile (not . cond) >> takeIf cond

findSeq :: (e -> Bool) -> Stream e [e]
findSeq cond = dropWhile (not . cond) >> takeWhile cond

takeIfMap :: (e -> Maybe a) -> Stream e (Maybe a)
takeIfMap func = do
    opt <- peek >>= (\maybeE -> return $ maybeE >>= func)
    when (isJust opt) drop >> return opt

takeWhileMap :: (e -> Maybe a) -> Stream e [a]
takeWhileMap func = do
    opt <- takeIfMap func
    case opt of
        Nothing -> return []
        Just e -> takeWhileMap func >>= (\es -> return $ e : es)

findMap :: (e -> Maybe a) -> Stream e (Maybe a)
findMap func = dropWhile (isNothing . func) >> takeIfMap func

findSeqMap :: (e -> Maybe a) -> Stream e [a]
findSeqMap func = dropWhile (isNothing . func) >> takeWhileMap func

either :: Stream e (Either l a) -> (a -> Stream e b) -> Stream e (Either l b)
either stream action = stream >>= \case Left l -> return (Left l); Right a -> fmap Right (action a)


either' :: Stream e (Either l a) -> (a -> Stream e (Either l b)) -> Stream e (Either l b)
either' stream action = stream >>= \case Left l -> return (Left l); Right a -> action a


class Propagate (p :: Type -> Type)  where
    propagate :: (Monad m) => m (p a) -> (a -> m (p b)) -> m (p b)

instance Propagate Maybe where
    propagate m1 m2 = m1 >>= \case Nothing -> return Nothing; Just a -> m2 a

instance Propagate (Either a) where
    propagate m1 m2 = m1 >>= \case Left e -> return (Left e); Right a -> m2 a

propagate' :: (Propagate p, Monad m) => m (p a) -> m (p b) -> m (p b)
propagate' m1 m2 = propagate m1 (const m2)