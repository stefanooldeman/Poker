module Cards where
-- Card module

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Ord, Eq, Enum)

instance Show Suit where
    show Spades = "S"
    show Hearts = "H"
    show Diamonds = "D"
    show Clubs = "C"

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum)

instance Show Value where 
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

data Card = Card {value :: Value, suit :: Suit}
    deriving (Eq)

instance Show Card where
    show (Card value suit) = show value ++ show suit

instance Ord Card where
    a > b = value a > value b
    a < b = value a < value b
    a <= b = value a < value b ||  a == b
    a >= b = value a > value b ||  a == b

-- next module Deck
