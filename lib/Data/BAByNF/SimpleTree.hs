module Data.BAByNF.SimpleTree where

import Data.Semigroup (sconcat)

import qualified Data.ByteString as BS (ByteString, concat, null, empty)
import qualified Data.List.NonEmpty as NE (NonEmpty (..), (<|), singleton, map, toList)

import Data.BAByNF.Grammar (RuleRef)

data Node = RefNode RuleRef Node | BranchNode (NE.NonEmpty Node) | LeafNode BS.ByteString deriving (Eq, Show)

class ToNode a where
    toNode :: a -> Node

getRef :: Node -> Maybe RuleRef
getRef (RefNode ref _) = Just ref 
getRef _ = Nothing

isParent :: Node -> Bool
isParent (RefNode _ _) = True
isParent (BranchNode _) = True
isParent (LeafNode _ ) = False

isRef :: Node -> Bool
isRef (RefNode _ _) = True
isRef _ = False

isBranch :: Node -> Bool
isBranch (BranchNode _) = True
isBranch _ = False

isLeaf :: Node -> Bool
isLeaf (LeafNode _) = True
isLeaf _ = False

getChildren :: Node -> [Node]
getChildren (LeafNode _) = []
getChildren (RefNode _ child) = [child]
getChildren (BranchNode children) = NE.toList children

stackSearch :: Node -> NE.NonEmpty (NE.NonEmpty Node) 
stackSearch = stackSearchBase []

stackSearchBase :: [Node] -> Node -> NE.NonEmpty (NE.NonEmpty Node) 
stackSearchBase base current@(LeafNode _) = NE.singleton (current NE.:| base)
stackSearchBase base current@(RefNode _ next) =
    let nextBase = current : base
     in (current NE.:| base) NE.<| (stackSearchBase nextBase next)
stackSearchBase base current@(BranchNode children) =
    let nextBase = current : base
     in (current NE.:| base) NE.<| (sconcat . (NE.map (stackSearchBase nextBase)) $ children)

simplify :: Node -> Node
simplify node@(LeafNode _) = node
simplify (RefNode ref child) = RefNode ref (simplify child)
simplify (BranchNode children) =
    let sc = trimEmpty . mergeLeafs . flatten . (NE.map simplify) $ children
     in case sc of
        Just (x NE.:| []) -> x
        Just xs -> BranchNode xs
        Nothing -> LeafNode $ BS.empty
    where flatten (first NE.:| rest) = case (first, rest) of
            (BranchNode c, []) -> c
            (n, []) -> NE.singleton n
            (BranchNode c, x:xs)  -> c <> (flatten $ x NE.:| xs)
            (n, x:xs) -> n NE.<| (flatten $ x NE.:| xs)
          trimEmpty :: NE.NonEmpty Node -> Maybe (NE.NonEmpty Node)
          trimEmpty nodes@(node@(LeafNode bs) NE.:| rest) =
            case (BS.null bs, rest) of
                (True, []) -> Nothing
                (True, (x:xs)) -> trimEmpty (x NE.:| xs)
                (False, []) -> Just nodes
                (False, (x:xs)) -> Just $ node NE.:| (maybe [] NE.toList $ trimEmpty (x NE.:| xs)) 
          trimEmpty nodes@(_ NE.:| []) = Just $  nodes
          trimEmpty (node NE.:| x : xs) = Just $ node NE.:| (maybe [] NE.toList $ trimEmpty (x NE.:| xs))  
          mergeLeafs :: NE.NonEmpty Node -> NE.NonEmpty Node
          mergeLeafs nodes@(node NE.:| rest) = case collectLeafs nodes of
            Just (bs, []) -> NE.singleton . LeafNode . BS.concat . NE.toList $ bs
            Just (bs, (n:ns)) -> (LeafNode . BS.concat . NE.toList $ bs) NE.<| (mergeLeafs (n NE.:| ns))
            Nothing -> case rest of
                [] -> nodes
                (n:ns) -> node NE.<| (mergeLeafs (n NE.:| ns))
          collectLeafs :: NE.NonEmpty Node -> Maybe (NE.NonEmpty BS.ByteString, [Node])
          collectLeafs (LeafNode b NE.:| []) = Just (NE.singleton b, [])
          collectLeafs (LeafNode b NE.:| (x:xs)) = case collectLeafs (x NE.:| xs) of
            Just (bs, rest) -> Just (b NE.<| bs, rest)
            Nothing -> Just (NE.singleton b, (x:xs))
          collectLeafs _ = Nothing



payload :: Node -> BS.ByteString
payload (LeafNode p) = p
payload (RefNode _ child) = payload child
payload (BranchNode children) = foldMap payload children 

data NodeContext = RefNodeContext RuleRef | BranchNodeContext [Node] [Node]

data NodeZipper = NodeZipper { zipperContexts :: [NodeContext], zipperFocus :: Node }

toZipper :: Node -> NodeZipper
toZipper node = NodeZipper { zipperContexts = [], zipperFocus = node }

fromZipper :: NodeZipper -> Node
fromZipper zipper = zipperFocus . unfocus $ zipper 

zipNode :: NodeContext -> Node -> Node
zipNode (RefNodeContext ruleRef) node = RefNode ruleRef node
zipNode (BranchNodeContext prev next) node =
    BranchNode (drainInto prev (node NE.:| next))
    where drainInto [] nonEmpty = nonEmpty
          drainInto (c:cs) nonEmpty = drainInto cs (c NE.<| nonEmpty)

unzipNode :: Node -> Maybe (NodeContext, Node)
unzipNode (LeafNode _) = Nothing
unzipNode (RefNode ref inner) = Just $ (RefNodeContext ref, inner)
unzipNode (BranchNode (h NE.:| t)) = Just $ (BranchNodeContext [] t, h)

focusOut :: NodeZipper -> Maybe NodeZipper
focusOut zipper = 
    let node = zipperFocus zipper
        contexts = zipperContexts zipper
     in case contexts of
        [] -> Nothing
        (c:cs) -> Just $ NodeZipper { zipperContexts = cs, zipperFocus = zipNode c node }

unfocus :: NodeZipper -> NodeZipper
unfocus zipper =
    case focusOut zipper of
        Nothing -> zipper
        Just outer -> unfocus outer

focusIn :: NodeZipper -> Maybe NodeZipper
focusIn zipper =
    case unzipNode (zipperFocus zipper) of
        Nothing -> Nothing
        Just (context, focus) -> Just (putFocus (pushContext zipper context) focus)

focusBack :: NodeZipper -> Maybe NodeZipper
focusBack zipper = case popContext zipper of
    Nothing -> Nothing
    Just (outer, oldContext) -> 
        let oldFocus = zipperFocus zipper
         in case focusBackContext oldContext oldFocus of
            Nothing -> Nothing
            Just (newContext, newFocus) -> Just $ putFocus (pushContext outer newContext) newFocus

focusBackContext :: NodeContext -> Node -> Maybe (NodeContext, Node)
focusBackContext (BranchNodeContext (newFocus:prev) next) oldFocus = Just (BranchNodeContext prev (oldFocus:next), newFocus)
focusBackContext _ _ = Nothing

focusForward :: NodeZipper -> Maybe NodeZipper
focusForward zipper = case popContext zipper of
    Nothing -> Nothing
    Just (outer, oldContext) -> 
        let oldFocus = zipperFocus zipper
         in case focusForwardContext oldContext oldFocus of
            Nothing -> Nothing
            Just (newContext, newFocus) -> Just $ putFocus (pushContext outer newContext) newFocus

focusForwardContext :: NodeContext -> Node -> Maybe (NodeContext, Node)
focusForwardContext (BranchNodeContext prev (newFocus:next)) oldFocus = Just (BranchNodeContext (oldFocus:prev) next, newFocus)
focusForwardContext _ _ = Nothing

pushContext :: NodeZipper -> NodeContext -> NodeZipper
pushContext zipper newContext =
    let NodeZipper { zipperContexts = contexts, zipperFocus = focus} = zipper
     in NodeZipper { zipperContexts = (newContext : contexts), zipperFocus = focus }

popContext :: NodeZipper -> Maybe (NodeZipper, NodeContext)
popContext zipper =
    let NodeZipper { zipperContexts = contexts, zipperFocus = focus} = zipper
     in case contexts of
        [] -> Nothing
        (c:cs) -> Just $ (NodeZipper { zipperContexts = cs, zipperFocus = focus}, c)


putFocus :: NodeZipper -> Node -> NodeZipper
putFocus zipper newFocus =
    let NodeZipper { zipperContexts = contexts, zipperFocus = _ } = zipper
     in NodeZipper { zipperContexts = contexts, zipperFocus = newFocus }

data WalkerState a = WalkerState NodeZipper a
data Walker a = Walker (NodeZipper -> WalkerState a)

instance Functor Walker where
    fmap func (Walker action) = Walker ((\(WalkerState zipper val) -> WalkerState zipper (func val)) . action)

instance Applicative Walker where
    pure val = Walker (\zipper -> WalkerState zipper val)
    liftA2 func (Walker action1) (Walker action2) = 
        Walker (\z1 -> 
            let WalkerState z2 a = action1 z1
                WalkerState z3 b = action2 z2
             in WalkerState z3 (func a b)
        )

instance Monad Walker where
    (>>=) (Walker action) toAction2 =
        Walker (\z1 ->
            let WalkerState z2 val = action z1
                Walker action2 = toAction2 val
             in action2 z2) 

data WalkDir = WalkIn | WalkOut | WalkBack | WalkForward
data WalkResult = WalkSuccess Node | WalkFailure

walk :: WalkDir -> Walker WalkResult
walk dir = Walker (\zipper -> 
        let tryMove = case dir of
                WalkOut -> focusOut
                WalkIn -> focusIn
                WalkBack -> focusBack
                WalkForward -> focusForward
            defaultResult = WalkerState zipper WalkFailure
            toNewState newZipper = WalkerState newZipper (WalkSuccess $ zipperFocus newZipper)
         in  maybe defaultResult toNewState (tryMove zipper)
    )

walkBack :: Walker WalkResult
walkBack = walk WalkBack

walkerFocus :: Walker Node
walkerFocus = Walker (\zipper -> WalkerState zipper (zipperFocus zipper))

walkForward :: Walker WalkResult
walkForward = walk WalkForward

walkIn :: Walker WalkResult
walkIn = walk WalkIn

walkOut :: Walker WalkResult
walkOut = walk WalkOut

replaceNode :: (Node -> Node) -> Walker ()
replaceNode func = Walker ((\z -> WalkerState z ()) . (\z -> let node = func . zipperFocus $ z in putFocus z node))
    
-- Todo: what if action modifies node??
visitAll :: (Node -> Walker ()) -> Walker ()
visitAll actOn = do
    node <- walkerFocus
    actOn node
    x <- walkIn
    case x of
        WalkSuccess node -> (visitAll actOn) >> walkOut >> return ()
        WalkFailure -> return ()
    visitAllForward actOn
    where visitAllForward :: (Node -> Walker ()) -> Walker ()
          visitAllForward actOn = do
            x <- walkForward
            case x of
                WalkSuccess node -> (visitAll actOn) >> (visitAllForward actOn)
                WalkFailure -> return ()
    


execWalker :: Walker a -> NodeZipper -> (NodeZipper, a)
execWalker (Walker action) inputZipper =
    let WalkerState outputZipper val = action inputZipper
     in (outputZipper, val)

execWalker' :: Walker a -> Node -> (Node, a)
execWalker' walker = (\(a, b) -> (fromZipper a, b) ) . (execWalker walker) . toZipper
execWalker_ :: Walker a -> NodeZipper -> NodeZipper
execWalker_ walker = fst . (execWalker walker)
execWalker'_ :: Walker a -> Node -> Node
execWalker'_ walker = fromZipper . (execWalker_ walker) . toZipper