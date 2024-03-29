module Deck where 

import System.IO.Unsafe
import System.Random
import Data.Map
import Data.List

import Cards

type Deck = [Card]

fullDeck :: Deck
fullDeck = [Card v s | v <-[Two .. Ace], s <-[Spades,Hearts,Diamonds,Clubs]]

shuffleDeck :: Deck -> Deck
shuffleDeck xs = parse_deck $ shuffle' xs (length xs)
    where 
        shuffle' _ 0    = return []
        shuffle' xs len = 
            do  n           <- randomRIO (0, len - 1)
                let (y, ys) =  choose n xs
                ys'         <- shuffle' ys (len - 1)
                return (y:ys')
        -- pick / choose method
        choose _ []     = error "choose: index out of range"
        choose 0 (x:xs) = (x, xs)
        choose i (x:xs) = let (y, ys) = choose (i - 1) xs in (y, x:ys)
        -- parse deck
        parse_deck :: IO Deck -> Deck
        parse_deck io = unsafePerformIO io

dealNCards :: Int -> Deck -> ([Card], Deck)
dealNCards n xxs = (sort $ take n xxs, shuffleDeck $ drop n xxs)

