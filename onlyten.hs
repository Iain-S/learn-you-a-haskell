import Control.Monad

mainOld = do
    contents <- getContents
    putStr $ shortLinesOnly contents

mainSimple = interact shortLinesOnly

-- mainSimple is more readable so this is only
-- to show function composition
oneLineMain = interact $ unlines . filter ((<10) . length) . lines

main = interact $ unlines . map palindrome . lines
  where palindrome x = if reverse x == x then "palindrome" else "not"

-- python equivalent might be
-- while True:
--   x = input()
--   if len(x) < 10:
--     print(x)

-- Note: Remember to filter if you only need some items.
-- Also Note: shortLinesOnly operates on real strings and not I/O.
shortLinesOnly :: String -> String
shortLinesOnly input = 
    let allLines = lines input
        shortLines = filter (\x -> length x < 10) allLines
    in unlines shortLines
