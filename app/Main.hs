module Main (main) where

import UI.App (run)
import System.Directory (getCurrentDirectory)
import Commands (isGitRepository)

main :: IO ()
main = do
  currentFolder <- getCurrentDirectory
  res <- isGitRepository currentFolder
  if res then run
  else putStrLn $ "There is no git repository here ["++ currentFolder ++"]."
