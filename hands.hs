module Hands where

import Cards
{-  
 - use the "record syntax" to define the combo data types.
 - for example a "Three of a kind"
 - the benefit: cards don't have to be in any order as long as there present in a _Hand_
 -}

type Hand = [Card]
data Combo = StraightFlush | FourOfAKind | FullHouse | Flush | 
             Straight | ThreeOfAKind | OnePair Card Card | HighCards
    deriving (Show, Ord, Eq)

isOnePair :: Hand -> Maybe Combo 
isOnePair (card1@(Card value1 _) :card2@(Card value2 _) :xs) = case value1 == value2 of
                                                           True  -> Just (OnePair card1 card2)
                                                           False -> isOnePair (card2:xs)
isOnePair (_:xs) = Nothing

-- quickcheck... isOnePair [Card {value = Two, suit = Spades }, Card {value = Two, suit = Hearts}]
-- [9C,5H,9S,KS,KD]
--
-- isOnePair [Card {value = Two, suit = Spades }, Card {value = Three, suit = Diamonds}, Card {value = Four, suit = Clubs}, Card {value = Two, suit = Hearts}, Card {value = Five, suit = Spades}]

