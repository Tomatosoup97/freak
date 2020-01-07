import System.Environment
import System.Exit
import Freak

usage = putStrLn "Usage: freak [filename]"

run :: [String] -> IO ()
run ["-h"] = usage >> exitSuccess
run [filename] = runFromFile filename
run _ = usage >> exitFailure

main :: IO ()
main = getArgs >>= run
