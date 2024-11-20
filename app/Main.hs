module Main (main) where

import qualified Brick.BChan  as BC
import qualified Graphics.Vty as V

import System.Directory (getCurrentDirectory)
import Repository.GitRepository
import Service.StateService
import Application
import Brick.Main
import Model.ApplicationEvent

main :: IO ()
main = do
  currentFolder <- getCurrentDirectory
  isGitRepo <- isGitDirectory gitRepo currentFolder
  if isGitRepo then run
  else putStrLn $ "There is no git repository here ["++ currentFolder ++"]."
  
run :: IO ()
run = do
  appEvChannel   <- BC.newBChan 5 :: IO (BC.BChan ApplicationEvent)
  let vtyBuilder = V.mkVty V.defaultConfig
  initialVty     <- vtyBuilder
  initialState   <- initState stateService
  _ <- customMain initialVty vtyBuilder (Just appEvChannel) (application appEvChannel) initialState
  pure ()
