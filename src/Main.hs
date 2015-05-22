import System.Environment

data Operation =
  None
  | New String
  | Invalid

main = do
  stringArgs <- getArgs
  let operation = parseArgs stringArgs
  perform operation

parseArgs [] = None
parseArgs ["new", arg] = New arg
parseArgs _ = Invalid

perform None = putStrLn "No arguments specified"
perform (New name) = newProject name
perform Invalid = putStrLn "Invalid arguments specified"

newProject name =
  putStrLn $ "New project: " ++ name
