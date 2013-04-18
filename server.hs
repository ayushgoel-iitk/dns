import Network
import Data.Char (toLower)
import Text.Regex.Posix ((=~))
import System.IO (hGetLine,hClose,hPutStrLn,hSetBuffering,BufferMode(..),Handle,stdout)
import Text.ParserCombinators.Parsec
import System.IO  
import Control.Monad

port = 2710 

f :: [String] -> [Int]
f = map read
main = withSocketsDo $ do
	putStrLn "Server Running"
	hSetBuffering stdout NoBuffering
        let list = []
        handle <- openFile "project.cabal" ReadMode
        contents <- hGetContents handle
        singlewords <- (words contents)
        list <- f singlewords
        print list
        hClose handle   
	 
	server
	putStrLn "Connection closed."
 

readIp = untilM	(=~ "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
	(putStr "Enter IP address: " >> getLine)
 
-- monadic `until`
untilM p x = x >>= (\y -> if p y then return y else untilM p x) 

while2 x y = ifM x (return ()) $ ifM y (return ()) $ while2 x y 
-- monadic `if`
ifM p t f  = p >>= (\p' -> if p' then t else f)
 
server = do
	sock <- listenOn (PortNumber port)
	putStrLn "Awaiting connection."
	(h,host,port) <- accept sock
	putStrLn $ "Received connection from " ++ host ++ ":" ++ show port
	hSetBuffering h LineBuffering
	while2 (receive h) (send h)
	hClose h
	sClose sock
 
send h = do
	putStr "Enter Domain : "
	input <- getLine
	hPutStrLn h input
	return $ null input


receive h = do
	putStr "Receiving: "
	input <- hGetLine h
	putStrLn input
	return $ null input
