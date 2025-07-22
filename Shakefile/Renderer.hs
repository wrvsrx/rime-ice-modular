{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=all #-}

module Shakefile.Renderer (
  rimeComponentToRule,
  rimeComponentToMap,
  rimeComponentToJSON,
  buildDir,
  srcDir,
) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Tree qualified as Tr
import Development.Shake
import Development.Shake.FilePath
import Shakefile.Components (
  RimeComponent,
  RimeTransformation (..),
 )

srcDir :: FilePath
srcDir = "externals" </> "rime-ice"

buildDir :: FilePath
buildDir = "build"

rimeTransformationToAction :: RimeTransformation -> Action ()
rimeTransformationToAction = \case
  RimeTransformationIdentity path -> copyFileChanged (srcDir </> path) (buildDir </> path)
  RimeTransformationRename path path' -> copyFileChanged (srcDir </> path) (buildDir </> path')
  RimeTransformationApply path path' f -> do
    content <- readFile' (srcDir </> path)
    writeFile' (buildDir </> path') (f content)
  RimeTransformationProduce path content -> writeFile' (buildDir </> path) content

rimeComponentToMap :: RimeComponent -> M.Map String ([RimeTransformation], [String])
rimeComponentToMap (Tr.Node (name, transformations) childern) =
  M.insert
    name
    (transformations, map (fst . Tr.rootLabel) childern)
    (M.unions (map rimeComponentToMap childern))

rimeComponentToRule :: String -> [RimeTransformation] -> [String] -> Rules ()
rimeComponentToRule name transformation_list dependencies =
  phony name $ do
    need dependencies
    mapM_ rimeTransformationToAction transformation_list

rimeComponentToJSON :: [RimeTransformation] -> [String] -> A.Value
rimeComponentToJSON transformation_list dependencies =
  let
    inputs =
      mapMaybe
        ( \case
            RimeTransformationIdentity path -> Just path
            RimeTransformationApply path _ _ -> Just path
            RimeTransformationProduce _ _ -> Nothing
            RimeTransformationRename path _ -> Just path
        )
        transformation_list
    outputs =
      map
        ( \case
            RimeTransformationIdentity path -> Just path
            RimeTransformationApply _ path' _ -> Just path'
            RimeTransformationProduce path' _ -> Just path'
            RimeTransformationRename _ path' -> Just path'
        )
        transformation_list
   in
    A.object
      [ "inputs" .= inputs
      , "outputs" .= outputs
      , "dependencies" .= dependencies
      ]
