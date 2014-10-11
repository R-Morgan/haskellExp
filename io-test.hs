
--main :: IO ()
main = do putStrLn "Enter some words!"
          inStr <- getLine
          let outStr = (show . length . words) inStr
          putStrLn $ "Your entry has " ++ outStr ++ " words in it!"
