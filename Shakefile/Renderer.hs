{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Werror=all #-}

module Shakefile.Renderer (
  renderRimeComponentClosure,
) where

import Data.Map qualified as M
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

renderRimeTransformationToAction :: RimeTransformation -> Action ()
renderRimeTransformationToAction = \case
  RimeTransformationIdentity path -> copyFileChanged (srcDir </> path) (buildDir </> path)
  RimeTransformationRename path path' -> copyFileChanged (srcDir </> path) (buildDir </> path')
  RimeTransformationApply path f -> do
    content <- readFile' (srcDir </> path)
    let
      (path', content') = f (path, content)
    writeFile' (buildDir </> path') content'
  RimeTransformationProduce path content -> writeFile' (buildDir </> path) content

renderRimeComponentToMap :: RimeComponent -> M.Map String ([RimeTransformation], [String])
renderRimeComponentToMap (Tr.Node (name, transformations) childern) = M.insert name (transformations, map (fst . Tr.rootLabel) childern) (M.unions (map renderRimeComponentToMap childern))

renderRimeComponentClosure :: RimeComponent -> Rules ()
renderRimeComponentClosure component = do
  let
    components = M.toList (renderRimeComponentToMap component)
  mapM_
    ( \(name, (transformations, dependencies)) -> phony name $ do
        need dependencies
        mapM_ renderRimeTransformationToAction transformations
    )
    components
