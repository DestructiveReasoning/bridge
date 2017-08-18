import Bridge
import Card

main :: IO ()
main = do
    d <- shuffleDeck newDeck
    let north = sortHand $ take 13 d
        east = sortHand $ take 13 . drop 13 $ d
        south = sortHand $ take 13 . drop 26 $ d
        west = sortHand $ take 13 . drop 39 $ d
        nhcp = countHCP north
        ehcp = countHCP east
        shcp = countHCP south
        whcp = countHCP west
    putStrLn $ "North:\t"    ++ (showHand north) ++ " -> " ++ show nhcp ++ " HCP"
    putStrLn $ "East:\t"     ++ (showHand east)  ++ " -> " ++ show ehcp ++ " HCP"
    putStrLn $ "South:\t"    ++ (showHand south) ++ " -> " ++ show shcp ++ " HCP"
    putStrLn $ "West:\t"     ++ (showHand west)  ++ " -> " ++ show whcp ++ " HCP"
