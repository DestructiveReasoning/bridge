module Card
(
 Card,
 Deck,
 Suit (..),
 compareValues,
 newDeck,
 printDeck,
 showCard,
 showHand,
 sortCards,
 sortHand,
 shuffleDeck
) where

import System.Random
import Debug.Trace

data Suit = S | H | D | C | NT deriving (Eq,Show)

type Card = (Int, Suit)
type Deck = [Card]

quicksort :: (a -> a -> Ordering) -> [a] -> [a]
quicksort _ [] = []
quicksort f (x:xs) = 
    let smaller = quicksort f $ filter (\n -> f n x == LT) xs
        greater = quicksort f $ filter (\n -> f n x /= LT) xs
    in greater ++ [x] ++ smaller

compareValues :: Card -> Card -> Ordering
compareValues (va,_) (vb,_)
    | va > vb = GT
    | va == vb = EQ
    | va < vb = LT

sortCards :: [Card] -> [Card]
sortCards = quicksort compareValues

sortHand :: Deck -> Deck
sortHand hand = 
    let spades      = sortCards $ filter (\(_,s) -> s == S) hand
        hearts      = sortCards $ filter (\(_,s) -> s == H) hand
        diamonds    = sortCards $ filter (\(_,s) -> s == D) hand
        clubs       = sortCards $ filter (\(_,s) -> s == C) hand
    in spades ++ hearts ++ diamonds ++ clubs

showCard :: Card -> String
showCard (v, suit)
    | v <= 10 = (show v) ++ (show suit)
    | v == 11 = "J" ++ (show suit)
    | v == 12 = "Q" ++ (show suit)
    | v == 13 = "K" ++ (show suit)
    | v == 14 = "A" ++ (show suit)

newDeck :: Deck
newDeck = [(x,y) | x <- [2..14], y <- [S,H,D,C]]

printDeck :: Deck -> IO ()
printDeck [] = return ()
printDeck (x:xs) = putStrLn (showCard x) >> printDeck xs

showHand :: Deck -> String
showHand deck = "[" ++ (iterate deck) ++ "]"
    where
    iterate [] = " "
    iterate (x:xs) = " " ++ (showCard x) ++ iterate xs

shuffleDeck :: Deck -> IO Deck
shuffleDeck d = 
    permute d $ (length d) * (length d)
    where
    permute deck 0 = pure deck
    permute deck x = do
        a <- getStdRandom (randomR (0, length deck - 2))
        b <- getStdRandom (randomR (a + 1, length deck - 1))
        let a' = if (a > b) then b else a
            b' = if (a > b) then a else b
            da = deck !! a'
            db = deck !! b'
            left    = take a' deck
            middle  = (drop (a' + 1) (take b' deck))
            right   = drop (b' + 1) deck
        permute ((take a' deck) ++ [db] ++ (drop (a'+1) (take b' deck)) ++ [da] ++ (drop (b'+1) deck)) (x-1)
