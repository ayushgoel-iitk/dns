import Network
import Data.Char (toLower)
import Text.Regex.Posix ((=~))
import System.IO (hGetLine,hClose,hPutStrLn,hSetBuffering,BufferMode(..),Handle,stdout)
 
port = 2710 
 
main = withSocketsDo $ do
	putStrLn "Client side"
	hSetBuffering stdout NoBuffering -- fix buffering under windows
	client
	putStrLn "Connection closed."
	putStrLn "Thanks for using One-Way Chat!" -- all done
 
-- reads in an ip address
readIp = untilM	(=~ "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
	(putStr "Enter IP address of the DNS server: " >> getLine)
 
-- monadic `until`
untilM p x = x >>= (\y -> if p y then return y else untilM p x) 
-- repeats two actions until either returns true
while2 x y = ifM x (return ()) $ ifM y (return ()) $ while2 x y 
-- monadic `if`
ifM p t f  = p >>= (\p' -> if p' then t else f)
 
-- client
client = do
	ip <- readIp
	putStrLn "Connecting..."
	h <- connectTo ip (PortNumber port)
	putStrLn $ "Connected to " ++ ip ++ ":" ++ show port
	hSetBuffering h LineBuffering
	while2 (send h) (receive h)
	hClose h
-- sending
send h = do
	putStr "Lookup: "
	input <- getLine
	hPutStrLn h input
	return $ null input
 
-- receiving
receive h = do
	putStr "Address: "
	input <- hGetLine h
	putStrLn input
	return $ null input
