module Deck where 

import System.Random
import Data.Map

import Cards

type Deck = [Card]
type Hand = [Card]

fullDeck :: Deck
fullDeck = [Card v s | v <-[Two .. Ace], s <-[Spades,Hearts,Diamonds,Clubs]]

shuffleDeck :: Deck -> IO Deck
shuffleDeck xs = shuffle' xs (length xs)
    where shuffle' _ 0    = return []
          shuffle' xs len = do n           <- randomRIO (0, len - 1)
                               let (y, ys) =  choose n xs
                               ys'         <- shuffle' ys (len - 1)
                               return (y:ys')

choose _ []     = error "choose: index out of range"
choose 0 (x:xs) = (x, xs)
choose i (x:xs) = let (y, ys) = choose (i - 1) xs in (y, x:ys)
