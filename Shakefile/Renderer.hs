{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Werror=all #-}

module Shakefile.Renderer (
  rimeComponent'ToRule,
  rimeComponentToMap,
) where

import Data.Map qualified as M
import Data.Tree qualified as Tr
import Development.Shake
import Development.Shake.FilePath
import Shakefile.Components (
  RimeComponent,
  RimeComponent' (..),
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

rimeComponent'ToRule :: RimeComponent' -> Rules ()
rimeComponent'ToRule component' =
  phony component'.name $ do
    need component'.dependencies
    mapM_ rimeTransformationToAction component'.transformation_list
