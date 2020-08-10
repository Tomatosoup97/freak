import System.Environment
import System.Exit
import Freak

-- TODO: extend usage with -p param
usage = putStrLn "Usage: freak [filename]"

run :: [String] -> IO ()
run ["-h"] = usage >> exitSuccess
run [filename] = runFromFile filename
run ["-p", filename] = parseFile filename
run _ = usage >> exitFailure

main :: IO ()
main = getArgs >>= run
