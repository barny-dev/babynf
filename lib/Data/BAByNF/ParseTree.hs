module Data.BAByNF.ParseTree where

import qualified Data.List.NonEmpty as NE ( NonEmpty((:|)), map, singleton)

import qualified Data.ByteString as BS

import qualified Data.BAByNF.Grammar as G
import qualified Data.BAByNF.SimpleTree as ST

data RuleNode = RuleNode
  { ruleRef :: G.RuleRef
  , ruleMatch :: BS.ByteString
  , ruleInner :: AltNode
  , ruleDef :: G.RuleDecl
  } deriving Eq

data AltNode = AltNode
  { altMatch :: BS.ByteString
  , altSelect :: ConcatNode
  , altSelectNo :: Integer
  , altDef :: G.Alt
  } deriving Eq

data ConcatNode = ConcatNode
  { concatMatch :: BS.ByteString
  , concatVals :: NE.NonEmpty RepNode
  , concatDef :: G.Concat
  } deriving Eq

data RepNode = RepNode
  { repMatch :: BS.ByteString
  , repInstances :: [ElemNode]
  , repDef :: G.Rep
  } deriving Eq

data ElemNode = ElemNode
  { elemMatch :: BS.ByteString
  , elemVal :: ElemNodeVal
  , elemDef :: G.Elem
  } deriving Eq

data ElemNodeVal = RuleNodeVal RuleNode | GroupNodeVal GroupNode | OptNodeVal OptNode | TermNodeVal TermNode deriving Eq

data GroupNode = GroupNode
  { groupMatch :: BS.ByteString
  , groupInner :: AltNode
  , groupDef :: G.Group
  } deriving Eq

data OptNode = OptNode
  { optMatch :: BS.ByteString
  , optInner :: Maybe AltNode
  , optDef :: G.Opt
  } deriving Eq

data TermNode = TermNode
  { termMatch :: BS.ByteString
  , termDef :: G.Term
  } deriving Eq

instance ST.ToNode ElemNode where
  toNode node =
    let child = case elemVal node of 
          RuleNodeVal a -> ST.toNode a
          GroupNodeVal a -> ST.toNode a
          OptNodeVal a -> ST.toNode a
          TermNodeVal a -> ST.toNode a
     in ST.BranchNode $ NE.singleton child

instance ST.ToNode RuleNode where
  toNode node =
    let child = ST.toNode (ruleInner node)
        ref = ruleRef node
     in ST.RefNode ref child

instance ST.ToNode GroupNode where
  toNode node =
    let child = ST.toNode (groupInner node)
     in ST.BranchNode $ NE.singleton child

instance ST.ToNode OptNode where
  toNode node =
    case optInner node of
      Nothing -> ST.LeafNode BS.empty
      Just alt ->
        let child = ST.toNode alt
         in ST.BranchNode $ NE.singleton child

instance ST.ToNode TermNode where
  toNode t = ST.LeafNode (termMatch t)

instance ST.ToNode AltNode where
  toNode node =
    let child = ST.toNode (altSelect node)
     in ST.BranchNode $ NE.singleton child

instance ST.ToNode ConcatNode where
  toNode node =
    let children = NE.map ST.toNode (concatVals node)
     in ST.BranchNode children

instance ST.ToNode RepNode where
  toNode node =
    let children = map ST.toNode (repInstances node)
     in case children of
      [] -> ST.LeafNode BS.empty
      (n:ns) -> ST.BranchNode $ (NE.:|) n ns
