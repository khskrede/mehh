
import System
import Data.List

data Test = Test Int

main = do
        let h = 'H':h
            e = "ello, world!"
            x = addStringToString (take 1 h) e
        putStrLn x


addStringToString :: String -> String -> String
addStringToString str1 str2 = str1 ++ str2
