module Deck where 
import Cards

type Deck = [Card]
type Hand = [Card]

fullDeck :: Deck
fullDeck = [Card v s | v <-[Two .. Ace], s <-[Spades,Hearts,Diamonds,Clubs]]


