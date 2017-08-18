module Bridge
(
 countHCP,
 playCard,
 Table
) where

import Card
import Data.List

--data Table = Table Deck Deck Deck Deck

data Team = NS | EW deriving (Eq, Show)

type Contract = (Int, Suit)
type Score = (Int, Team)

data Hand = AI Deck | Player Deck
data Cardinal = North | East | South | West deriving (Eq, Show)
type Table = (Hand, Hand, Hand, Hand)

getCards :: Hand -> Deck
getCards (AI d) = d
getCards (Player d) = d

compareSuits :: Suit -> Suit -> Ordering
compareSuits NT _   = GT
compareSuits S NT   = LT
compareSuits S _    = GT
compareSuits H NT   = LT
compareSuits H S    = LT
compareSuits H _    = GT
compareSuits D C    = GT
compareSuits D _    = LT
compareSuits C _    = LT

compareContracts :: Contract -> Contract -> Ordering
compareContracts (i1, s1) (i2, s2) = 
    if i1 < i2 then LT
    else if i1 > i2 then GT
    else compareSuits s1 s2

countHCP :: Deck -> Int
countHCP = count 0 
    where
    count n [] = n
    count n ((v,_):xs) = if v > 10 then count (n + v - 10) xs else count n xs

getWinner :: Contract -> Suit -> [Card] -> Int
getWinner (_, trump) suit cards =
    let trumps = sortCards $ filter (\(_,x) -> x == trump) cards
        winner = if (length trumps) == 0 then head . sortCards $ filter (\(_,x) -> x == suit) cards
                 else head trumps
        index  = elemIndex winner cards
    in
    case index of
        Just p -> p
        _      -> 0

getSuit :: [Card] -> Maybe Suit
getSuit [] = Nothing
getSuit cards = Just (snd . head $ cards)

playTrick :: Contract -> Int -> Table -> IO Table
playTrick contract start (north, east, south, west) =
    let players     = [north, east, south, west]
        players'    = shiftList start players
    in do 
        cards <- playCards contract [] players'
        let north'  = removeCard (cards !! 0) north
            east'   = removeCard (cards !! 1) east
            south'  = removeCard (cards !! 2) south
            west'   = removeCard (cards !! 3) west
        pure (north', east', south', west')
    where 
    shiftList 0 l = l
    shiftList n (x:xs) = shiftList (n-1) (xs ++ [x])
    playCards _ cards [] = pure $ shiftList (4 - start) cards
    playCards contract cards (x:xs) =
        playCard contract (getSuit cards) cards x >>= (\c -> playCards contract (cards ++ [c]) xs)
    removeCard card (AI deck) = let deck' = card:(filter (/= card) deck) in AI deck
    removeCard card (Player deck) = let deck' = card:(filter (/= card) deck) in Player deck

playCard :: Contract -> Maybe Suit -> [Card] -> Hand -> IO Card
playCard contract suit cards (AI hand) = calculateMove contract suit cards (AI hand)
playCard contract suit cards (Player hand) = undefined

calculateMove :: Contract -> Maybe Suit -> [Card] -> Hand -> IO Card
calculateMove = undefined
