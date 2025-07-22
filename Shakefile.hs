{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Werror=all #-}

module Shakefile (main) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.ByteString.Lazy.UTF8 qualified as BU
import Data.Map qualified as M
import Development.Shake
import Shakefile.Components (allComponent)
import Shakefile.Renderer (
  rimeComponentToJSON,
  rimeComponentToMap,
  rimeComponentToRule,
 )

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["all"]
  let
    components' = M.toList (rimeComponentToMap allComponent)
  mapM_ (\(name, (transformation_list, dependencies)) -> rimeComponentToRule name transformation_list dependencies) components'
  phony "json" $
    writeFileChanged
      "components.json"
      ( BU.toString $
          A.encode $
            A.object $
              map
                ( \(name, (transformation_list, dependencies)) ->
                    A.fromString name .= rimeComponentToJSON transformation_list dependencies
                )
                components'
      )
