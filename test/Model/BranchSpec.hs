{-# LANGUAGE OverloadedStrings #-}
module Model.BranchSpec where

import qualified Data.Text as T

import Model.Branch

import Test.Hspec
  ( shouldBe
  , parallel
  , it
  , describe
  , Spec
  )

spec :: Spec
spec = parallel $ do
  describe "Brnach.makeBranch" $ do
    it "make BranchRemote from `remoteLine` with correct origin and name" $ do
      let branch = makeBranch remoteLine
      branch `shouldBe` Just (BranchRemote "origin" "#1274472_move_all_circle_lookups_to_case_class")

    it "make BranchLocal from `localLine` with correct name" $ do
      let branch = makeBranch localLine
      branch `shouldBe` Just (BranchLocal "#1270873_The_product_should_be_customizable_for_case_cancellation")

    it "make BranchCurrent from `currentLine` with correct name" $ do
      let branch = makeBranch currentLine
      branch `shouldBe` Just (BranchCurrent "master")

    it "return Nothing for wrong branch line" $ do
      let branch = makeBranch wrongLine
      branch `shouldBe` Nothing

    it "return Nothing for `headLine`" $ do
      let branch = makeBranch headLine
      branch `shouldBe` Nothing

remoteLine :: T.Text
remoteLine = "  refs/remotes/origin/#1274472_move_all_circle_lookups_to_case_class 2024-11-14"

localLine :: T.Text
localLine = "  refs/heads/#1270873_The_product_should_be_customizable_for_case_cancellation 2024-10-22"

currentLine :: T.Text
currentLine = "* refs/heads/master 2024-10-25"

wrongLine :: T.Text
wrongLine = "   refs/wrong/branch_name"

headLine :: T.Text
headLine = "  refs/remotes/origin/HEAD 2024-10-25"
