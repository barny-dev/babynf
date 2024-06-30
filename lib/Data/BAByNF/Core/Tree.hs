module Data.BAByNF.Core.Tree where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

import Data.BAByNF.Core.Ref (Ref)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.BAByNF.Core.Ref as Ref

data Tree a where Tree :: Ref a => [Node a] -> Tree a

deriving instance (Eq a) => Eq (Tree a)
deriving instance (Show a) => Show (Tree a)

data Node a where
    StringNode :: ByteString -> Node a
    RefNode :: (Ref a) => a -> Tree a -> Node a

deriving instance (Eq a) => Eq (Node a)
deriving instance (Show a) => Show (Node a)

instance Semigroup (Tree a) where
    (<>) :: Tree a -> Tree a -> Tree a
    (<>) (Tree a) (Tree b) = Tree $ a ++ b

nodes :: (Ref a) => Tree a -> [Node a]
nodes (Tree ns) = ns

empty :: (Ref a) => Tree a
empty = Tree []

singleton :: (Ref a) => Node a -> Tree a
singleton node = Tree [node]

asSingleton :: (Ref a) => Tree a -> Maybe (Node a)
asSingleton (Tree [x]) = Just x
asSingleton _ = Nothing

stringify :: Tree a -> ByteString
stringify (Tree ns) = ByteString.concat . map stringifyNode $ ns

stringifyNode :: Node a -> ByteString
stringifyNode (RefNode _ tree) = stringify tree
stringifyNode (StringNode bs) = bs

mergeStrings :: Tree a -> Tree a
mergeStrings (Tree ns) = Tree $ merge ns
    where merge [] = []
          merge [x] = [mergeStringsInNode x]
          merge (x:xs) = mergeStringsInNode x : merge xs


mergeStringsInNode :: Node a -> Node a
mergeStringsInNode (RefNode ref tree) = RefNode ref (mergeStrings tree)
mergeStringsInNode node = node

dropRefs :: [a] -> Tree a -> Tree a
dropRefs refs (Tree ns) = Tree $ ns >>= applyDrop
    where applyDrop node@(StringNode _) = [node]
          applyDrop (RefNode ref tree) =
            let tree'@(Tree ns') = dropRefs refs tree
             in if any (Ref.eq ref) refs
                then ns'
                else [RefNode ref tree']

getChildrenWithRef :: a -> Tree a -> [Tree a]
getChildrenWithRef ref (Tree ns) = ns >>= filterOnRef
    where filterOnRef (RefNode ref' subtree) = [subtree | Ref.eq ref ref']
          filterOnRef _ = []

getChildWithRef :: a -> Tree a -> Maybe (Tree a)
getChildWithRef ref tree =
    case getChildrenWithRef ref tree of
        [] -> Nothing
        x : _ -> Just x

tryGetChildWithRef :: (Ref a) => a -> Tree a -> Either String (Tree a)
tryGetChildWithRef ref tree = 
    case getChildWithRef ref tree of
        Nothing -> Left $ "no subtree with ref <" ++ Ref.display ref ++ "> defined"
        Just subtree -> Right subtree

getDescendantsWithPath :: (Ref a) => NonEmpty a -> Tree a -> [Tree a]
getDescendantsWithPath (r :| rs) tree =
    let matching = getChildrenWithRef r tree
     in case rs of
        [] -> matching
        r' : rs' -> matching >>= getDescendantsWithPath (r' :| rs')
getFirstDescendantWithPath :: (Ref a) => NonEmpty a -> Tree a -> Maybe (Tree a)
getFirstDescendantWithPath refs tree = case getDescendantsWithPath refs tree of
    [] -> Nothing
    x:_ -> Just x

tryGetFirstPath :: (Ref a) => NonEmpty a -> Tree a -> Either String (Tree a)
tryGetFirstPath (r :| rs) tree =
    let e = tryGetChildWithRef r tree
     in case rs of
        [] -> e
        (r':rs') -> e >>= tryGetFirstPath (r' :| rs')   

getSubtreeIfRef :: (Ref a) => a -> Node a -> Maybe (Tree a)
getSubtreeIfRef ref (RefNode ref' subtree) = if Ref.eq ref ref' then Just subtree else Nothing
getSubtreeIfRef _ _ = Nothing

isStringEq :: (Ref a) => Node a -> ByteString -> Bool
isStringEq (StringNode bs) bs' = bs == bs' 
isStringEq _ _ = False

isRefOf :: (Ref a) => Node a -> a -> Bool 
isRefOf (RefNode ref _) ref' = Ref.eq ref ref'
isRefOf _ _ = False