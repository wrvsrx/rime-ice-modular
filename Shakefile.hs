{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Werror=all #-}

module Shakefile (main) where

import Development.Shake
import Shakefile.Components (allComponent)
import Shakefile.Renderer (renderRimeComponentClosure)

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["all"]
  renderRimeComponentClosure allComponent
