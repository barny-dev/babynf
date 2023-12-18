module Data.BAByNF.Core.Repeat
    ( Repeat
    , RepeatCount
    , required
    , optional
    , from
    , exactly
    , once
    , upTo
    , maybeOnce
    , zeroOrMore
    , oneOrMore
    , nOrMore
    , repeatDef
    , count
    , initCount
    , State (..)
    , tryIncrementCount
    , state
    ) where
import Numeric.Natural (Natural)


data Repeat = Repeat { required :: Natural, optional :: Maybe Natural } deriving (Eq, Show)

from :: Natural -> Maybe Natural -> Repeat
from req opt = Repeat { required = req, optional = opt }

exactly :: Natural -> Repeat
exactly val = Repeat { required = val, optional = Just 0}

once :: Repeat
once = exactly 1

upTo :: Natural -> Repeat
upTo val = Repeat { required = 0, optional = Just val }

maybeOnce :: Repeat
maybeOnce = upTo 1 

zeroOrMore :: Repeat
zeroOrMore = nOrMore 0

oneOrMore :: Repeat
oneOrMore = nOrMore 1

nOrMore :: Natural -> Repeat
nOrMore val = Repeat { required = val, optional = Nothing } 


initCount :: Repeat -> RepeatCount
initCount r = RepeatCount { repeatDef = r, count = 0 }

data RepeatCount = RepeatCount { repeatDef :: Repeat, count :: Natural } deriving (Eq, Show)

tryIncrementCount :: RepeatCount -> Maybe RepeatCount
tryIncrementCount x = 
    case state x of
        Satisfied -> Nothing
        _ -> Just $ RepeatCount { repeatDef = repeatDef x, count = count x + 1 }

data State = NeedMore | WantMore | Satisfied
state :: RepeatCount -> State
state RepeatCount { repeatDef = Repeat { required = required, optional = maybeOptional }, count = count} =
    if count < required then NeedMore else
    case maybeOptional of
        Nothing -> WantMore
        Just optional -> if optional > (count - required) then WantMore else Satisfied
