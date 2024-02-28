import System.Directory (listDirectory)


main :: IO ()
main = do
    putStrLn "Enter folder of text files to combine:"
    path <- getLine 
    putStrLn "Enter new file name"
    newtxt <- getLine
    texts <- getTexts path
    writeFile (path ++ "\\" ++  newtxt ++ ".txt") (combineTexts texts)
    putStrLn "type anything and press enter to exit"
    getLine
    return ()  


combineTexts :: [String] -> String
combineTexts (x:xs:xss) = x ++ "\n-------\n" ++ combineTexts (xs:xss)
combineTexts [x] = x
combineTexts _ = [] 

getTexts :: FilePath -> IO [String]
getTexts path = do 
    paths <- listDirectory path
    let paths2 = map ((path ++ "\\") ++) paths
    mapM readFile $ filter (elem ".txt" . tails) paths2 


tails :: [a] -> [[a]]
tails (x:xs) = (x:xs):tails xs
tails _ = [[]]

-- input : folder location
-- output : nothing (should have a txt file written 
--          that concats all txt in folder)