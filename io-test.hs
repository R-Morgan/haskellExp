
--main :: IO ()
main = do putStrLn "Enter some words!"
          inStr <- getLine
          let outStr = (show . length . words) inStr
          putStrLn outStr
