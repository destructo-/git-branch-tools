module Main (main) where

import System.Directory (getCurrentDirectory)
import Repository.GitRepository
import Service.StateService
import Application
import Brick.Main
import qualified Brick.BChan as BC

main :: IO ()
main = do
  currentFolder <- getCurrentDirectory
  isGitRepo <- isGitDirectory gitRepo currentFolder
  if isGitRepo then run
  else putStrLn $ "There is no git repository here ["++ currentFolder ++"]."
  
run :: IO ()
run = do
  initialState <- initState stateService
  _ <- defaultMain application initialState
  pure ()
