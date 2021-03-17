import Data.List
import Data.String
import System.Environment
import System.Exit
import System.Posix
import Text.Read

rule30 :: Int -> Int -> Int -> Int
rule30 p q r = mod (p + q + r + q * r) 2

rule90 :: Int -> Int -> Int -> Int
rule90 p q r = mod (p + r) 2

rule110 :: Int -> Int -> Int -> Int
rule110 p q r = mod (q + r + q * r + p * q * r) 2

rulz :: Int -> Int -> Int -> [Int] -> [Int] 
rulz a rule start list = case (a == 0) of
    True -> [printRule rule (0, list !! a, list !! (a + 1))]
    False -> case (start == 0) of
        True -> (rulz (a-1) rule (start+1) list) ++ [printRule rule (list !! (a - 1), list !! a, 0)]
        False -> (rulz (a-1) rule start list) ++ [printRule rule (list !! (a - 1), list !! a, list !! (a + 1))]

decal :: Int -> IO()
decal a = case (a > 0) of
    True -> do
        putStr " "
        decal (a - 1)
    False -> return ()

printRule :: Int -> (Int, Int, Int) -> Int
printRule rule (a, b, c) = case rule of
                30 -> rule30 a b c 
                90 -> rule90 a b c
                110 -> rule110 a b c
                rule -> 0


startloop :: (Int, Int) -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> Int -> IO()
startloop (a, b) c d rule window list newlist count = case (count == c) of
    True -> return ()
    False -> case (a == window + 300 - d) of --margin positive
            True -> do
                case (count >= b) of
                    True -> putStrLn ""
                    False -> putStr ""
                startloop (300 - d, b) c d rule window (rulz ((300+(div window 2)+b+count+1)*2) rule 0 list) [] (count+1)
            False -> do
                case (count >= b) of
                    True -> do
                        case (a == 300) of
                            True -> decal (d-1)
                            False -> putStr ""
                        case (list !! a) of
                            0 -> putStr " "
                            1 -> putStr "*"
                    False -> putStr ""
                startloop ((a + 1), b) c d rule window list newlist (count)

findRule :: [String] -> String -> Maybe Int
findRule (x:y:z) str = case (x == str) of
    True -> case y of
        [] -> Nothing
        _ -> readMaybe (y) :: Maybe Int
    False -> findRule (y:z) str
findRule (x:y) str = case (x == str) of
    True -> Nothing
    False -> Just 0
findRule [] str = Just 0


finWindow :: [String] -> Maybe Int
finWindow args = case (findRule args "--window" == Just 0) of
        True -> Just 80
        False -> findRule args "--window"

zeros :: Int -> [Int]
zeros a = case (a == 0) of
    True -> []
    False -> [0] ++ zeros (a - 1)

createlist :: Int -> Int -> [Int]
createlist window margin = case (margin < 0) of
        False -> (zeros ((div window 2) + 300)) ++ [1] ++ (zeros ((div window 2) + 300))
        True -> (zeros ((div window 2) + 300 + margin)) ++ [1] ++ (zeros ((div window 2) + 300 - margin))

fromPatern :: Maybe Int -> Int
fromPatern (Just i) = i
fromPatern Nothing = 0

main = do
    args <- getArgs

    case (findRule args "--rule") of
        Nothing -> do
            putStrLn "invalid args"
            exitWith (ExitFailure 84)
        Just i -> do
            return ()
    let rule = fromPatern (findRule args "--rule") 

    case (findRule args "--start") of
        Nothing -> do
            putStrLn "invalid args"
            exitWith (ExitFailure 84)
        Just i -> do
            return ()
    let start = fromPatern (findRule args "--start") 


    case (findRule args "--lines") of
        Nothing -> do
            putStrLn "invalid args"
            exitWith (ExitFailure 84)
        Just i -> do
            return ()
    let nblines = fromPatern (findRule args "--lines") 

    case (findRule args "--move") of
        Nothing -> do
            putStrLn "invalid args"
            exitWith (ExitFailure 84)
        Just i -> do
            return ()
    let margin = fromPatern (findRule args "--move")
        

    case (finWindow args) of
        Nothing -> do
            putStrLn "invalid args"
            exitWith (ExitFailure 84)
        Just i -> do
            return ()
    let window = fromPatern (finWindow args)  

    -- 40 IS A PLACEHOLDER
    let list = createlist window margin

    case ((rule /= 30) && (rule /= 90) && (rule /= 110)) of
        True -> do
            putStrLn "invalid args"
            exitWith (ExitFailure 84)
        False-> putStr ""
    case (nblines == 0) of
        False -> do
            startloop (300 - margin, 0 + start) (nblines + start) margin rule window list (rulz ((300+(div window 2)+start)*2) rule 0 list) 0
        True -> do
            let nblines = -1
            startloop (300 - margin, 0 + start) nblines margin rule window list (rulz ((300+(div window 2)+start)*2) rule 0 list) 0
    exitWith (ExitSuccess)