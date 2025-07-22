{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Werror=all #-}

module Shakefile (main) where

import Data.Aeson qualified as A
import Data.ByteString.Lazy.UTF8 qualified as BU
import Data.Map qualified as M
import Development.Shake
import Shakefile.Components (RimeComponent' (..), allComponent)
import Shakefile.Renderer (
  rimeComponent'ToRule,
  rimeComponentToMap,
 )

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["all"]
  let
    components' =
      map
        ( \(name, (transformation_list, dependencies)) ->
            RimeComponent'{name = name, transformation_list = transformation_list, dependencies = dependencies}
        )
        (M.toList (rimeComponentToMap allComponent))
  mapM_ rimeComponent'ToRule components'
  phony "json" $
    writeFileChanged "components.json" (BU.toString $ A.encode components')
