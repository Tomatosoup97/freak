import System.Environment
import System.Exit
import Freak

usage = putStrLn "\
    \Usage: freak [options] [filename]\n\
    \Options:\n\
    \                       Evaluate program\n\
    \   -h, --help          Print this message and exit\n\
    \   -c, --cps           CPS-translate program\n\
    \   -p, --parse         Parse program\n\
    \   -ds, --desugar      Parse and run AST transformations\n\
    \   -v, --validate      Run available static analysis\n"

run :: [String] -> IO ()
run ("-h":_) = usage >> exitSuccess
run ("--help":_) = run ["-h"]

run [filename] = runFromFile filename

run ["-p", filename] = parseFile filename
run ["--parse", filename] = run ["-p", filename]

run ["-ds", filename] = parseDesugarFile filename
run ["--desugar", filename] = run ["-ds", filename]

run ["-c", filename] = cpsFromFile filename
run ["--cps", filename] = run ["-c", filename]

run ["-v", filename] = validateFile filename
run ["--validate", filename] = run ["-v", filename]

run _ = usage >> exitFailure

main :: IO ()
main = getArgs >>= run
