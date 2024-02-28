import System.Directory (listDirectory)


main :: IO ()
main = do
    putStrLn "Enter folder of text files to combine:"
    path <- getLine
    let clean = cleanpath path 
    putStrLn "Enter new file name"
    newtxt <- getLine
    texts <- getTexts clean
    writeFile (clean ++ "\\" ++  newtxt ++ ".txt") (combineTexts texts)
    --delete these next two lines and recompile
    putStrLn "type anything and press enter to exit"
    getLine
    return ()  


combineTexts :: [String] -> String
combineTexts (x:xs:xss) = x ++ "\n-------\n" ++ combineTexts (xs:xss)
combineTexts [x] = x
combineTexts _ = [] 

cleanpath :: String -> String
cleanpath ('\"':xs) = cleanpath xs
cleanpath (x:xs) = x:cleanpath xs
cleanpath _ = []

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
