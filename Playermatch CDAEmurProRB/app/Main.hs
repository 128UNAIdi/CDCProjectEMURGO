import Data.List (sort, sortBy, transpose)
import Data.Function (on)
import Control.Monad.Trans.State.Lazy (modify, execState)


type PlayerPair = (String, Int)



main :: IO()
main = gameStart []

gameStart :: [PlayerPair] -> IO()
gameStart oldPlayer = do
                     putStrLn "(i) Input player name & rating (m) Make teams and balance (q) Quit"
                     stepChoice <- getLine
                     case stepChoice of
                        "i"  -> do
                                   putStrLn "Player Name ? "
                                   playerName <- getLine
                                   putStrLn "Player Rating ? (0-100) "
                                   playerRating <- readLn
                                   let newPlayerList = oldPlayer ++ [(playerName, playerRating)]
                                   print newPlayerList
                                   putStr "Total player(s) : "
                                   print (length newPlayerList)
                                   gameStart  newPlayerList
                        "m"  -> do
                                   putStrLn "Make how many teams? "
                                   nTeams <- readLn
                                   let oldPlayerMT = execState g oldPlayer'
                                       oldPlayer' = reverseEvenPostn $ group nTeams $ sortBy (flip compare `on` snd) oldPlayer
                                       reverseEvenPostn :: [[PlayerPair]] -> [[PlayerPair]]
                                       reverseEvenPostn  [] = []
                                       reverseEvenPostn  [[]] = [[]]
                                       reverseEvenPostn  [x] = [x]
                                       reverseEvenPostn (x:y:xs) = x : reverse y : reverseEvenPostn xs
                                       group :: Int -> [PlayerPair] -> [[PlayerPair]]
                                       group _ [] = []
                                       group n l
                                        | n > 0 = take n l : group n (drop n l)
                                        | otherwise = error "Negative or zero n"
                                   putStrLn " Balanced teams are as below: Nth List -> Team N"
                                   print oldPlayerMT
                                   let totalRating = map (sum . map snd) oldPlayerMT
                                   putStrLn " Rating of each respective teams are : "
                                   print totalRating
                                   putStrLn " Teams had been logged! Please check logTeam.txt for history. Thank you!"
                                   appendFile "logTeam.txt" "\n Teams: \n"
                                   appendFile "logTeam.txt" (show oldPlayerMT)
                                   appendFile "logTeam.txt" " ; Respective relative cumulative strength :"
                                   appendFile "logTeam.txt" (show totalRating)
                                   appendFile "logTeam.txt" " ; Total number of teams :"
                                   appendFile "logTeam.txt" (show (length oldPlayerMT))
                                        where g = do
                                                  modify transpose
                        "q"    -> do
                                   putStrLn "Thank you for using our service!"
                        _    -> do
                                   putStrLn "Please choose from indexing letters in the menu!"
                                   print oldPlayer
                                   putStr "Total player(s) : "
                                   print (length oldPlayer)
                                   gameStart oldPlayer
