{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Werror=all #-}

module Shakefile.Components (
  RimeTransformation (..),
  RimeComponent,
  allComponent,
) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.ByteString.UTF8 qualified as BU
import Data.List.Extra (replace)
import Data.Tree qualified as Tr
import Data.Yaml qualified as Y
import Text.Printf (printf)
import Text.RawString.QQ (r)

-- 有几种可能：
--
-- - 直接复制文件且相对路径不变
-- - 直接复制文件且相对路径改变
-- - 经过变换生成文件
-- - 直接写入文件
--
-- 我们不存在通过外部软件处理文件的情况

data RimeTransformation
  = RimeTransformationIdentity FilePath
  | RimeTransformationRename FilePath FilePath
  | RimeTransformationApply FilePath FilePath (String -> String)
  | RimeTransformationProduce FilePath String

type RimeComponent = Tr.Tree (String, [RimeTransformation])

opencc :: RimeComponent
opencc =
  Tr.Node
    ( "opencc"
    ,
      [ RimeTransformationIdentity "opencc/emoji.json"
      , RimeTransformationIdentity "opencc/emoji.txt"
      , RimeTransformationIdentity "opencc/others.txt"
      ]
    )
    []

luas :: RimeComponent
luas =
  Tr.Node
    ( "lua"
    ,
      [ RimeTransformationIdentity "lua/autocap_filter.lua"
      , RimeTransformationIdentity "lua/calc_translator.lua"
      , RimeTransformationIdentity "lua/cn_en_spacer.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/drop_words.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/filter.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/hide_words.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/logger.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/metatable.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/processor.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/reduce_freq_words.lua"
      , RimeTransformationIdentity "lua/cold_word_drop/string.lua"
      , RimeTransformationIdentity "lua/corrector.lua"
      , RimeTransformationIdentity "lua/date_translator.lua"
      , RimeTransformationIdentity "lua/debuger.lua"
      , RimeTransformationIdentity "lua/en_spacer.lua"
      , RimeTransformationIdentity "lua/force_gc.lua"
      , RimeTransformationIdentity "lua/is_in_user_dict.lua"
      , RimeTransformationIdentity "lua/long_word_filter.lua"
      , RimeTransformationIdentity "lua/lunar.lua"
      , RimeTransformationIdentity "lua/number_translator.lua"
      , RimeTransformationIdentity "lua/pin_cand_filter.lua"
      , RimeTransformationIdentity "lua/reduce_english_filter.lua"
      , RimeTransformationIdentity "lua/search.lua"
      , RimeTransformationIdentity "lua/select_character.lua"
      , RimeTransformationIdentity "lua/t9_preedit.lua"
      , RimeTransformationIdentity "lua/unicode.lua"
      , RimeTransformationIdentity "lua/v_filter.lua"
      ]
    )
    []

meltEngSchema :: RimeComponent
meltEngSchema =
  Tr.Node
    ( "melt_eng"
    ,
      [ RimeTransformationIdentity "melt_eng.dict.yaml"
      , RimeTransformationIdentity "melt_eng.schema.yaml"
      , RimeTransformationIdentity "en_dicts/en.dict.yaml"
      , RimeTransformationIdentity "en_dicts/en_ext.dict.yaml"
      ]
    )
    []

radicalPinyinSchema :: RimeComponent
radicalPinyinSchema =
  Tr.Node
    ( "radical_pinyin"
    ,
      [ RimeTransformationIdentity "radical_pinyin.dict.yaml"
      , RimeTransformationIdentity "radical_pinyin.schema.yaml"
      ]
    )
    []

rimeIceSuggestion :: RimeComponent
rimeIceSuggestion =
  Tr.Node
    ( "suggestion"
    , [RimeTransformationRename "default.yaml" "rime_ice_suggestion.yaml"]
    )
    []

rimeIceDoubleDefaultPatch :: RimeComponent
rimeIceDoubleDefaultPatch =
  Tr.Node
    ( "double-default-patch"
    ,
      [ RimeTransformationProduce
          "rime_ice_double_pinyin_default_patch.yaml"
          ( (BU.toString . Y.encode . A.object)
              [ "__merge" .= A.object ["recognizer/patterns/uppercase" .= A.Null]
              , "__include" .= ("rime_ice_default_patch:/" :: String)
              ]
          )
      ]
    )
    []

rimeIceDefaultPatch :: RimeComponent
rimeIceDefaultPatch =
  Tr.Node
    ( "default-patch"
    ,
      [ RimeTransformationProduce
          "rime_ice_default_patch.yaml"
          ( (BU.toString . Y.encode . A.object)
              [ "switcher/save_options/+" .= (["ascii_mode", "emoji", "search_single_char", "traditionalization"] :: [String])
              , "punctuator" .= A.object ["__include" .= ("rime_ice_suggestion:/punctuator" :: String)]
              , "key_binder/select_first_character" .= A.object ["__include" .= ("rime_ice_suggestion:/key_binder/select_first_character" :: String)]
              , "key_binder/select_last_character" .= A.object ["__include" .= ("rime_ice_suggestion:/key_binder/select_last_character" :: String)]
              ]
          )
      ]
    )
    [rimeIceSuggestion]

vSymbols :: RimeComponent
vSymbols = Tr.Node ("symbols_v", [RimeTransformationIdentity "symbols_v.yaml"]) []

vCapSymbols :: RimeComponent
vCapSymbols = Tr.Node ("symbols_caps_v", [RimeTransformationIdentity "symbols_caps_v.yaml"]) []

rimeIcePinyinSchema :: RimeComponent
rimeIcePinyinSchema =
  Tr.Node
    ( "pinyin"
    ,
      [ RimeTransformationIdentity "rime_ice.schema.yaml"
      , RimeTransformationIdentity "rime_ice.dict.yaml"
      , RimeTransformationIdentity "cn_dicts/41448.dict.yaml"
      , RimeTransformationIdentity "cn_dicts/8105.dict.yaml"
      , RimeTransformationIdentity "cn_dicts/base.dict.yaml"
      , RimeTransformationIdentity "cn_dicts/ext.dict.yaml"
      , RimeTransformationIdentity "cn_dicts/others.dict.yaml"
      , RimeTransformationIdentity "cn_dicts/tencent.dict.yaml"
      , RimeTransformationIdentity "custom_phrase.txt"
      , RimeTransformationIdentity "en_dicts/cn_en.txt"
      ]
    )
    [ luas
    , rimeIceDefaultPatch
    , opencc
    , meltEngSchema
    , radicalPinyinSchema
    , vSymbols
    ]

data DoublePinyinSchema
  = Natural
  | Flypy
  | ABC
  | MSPY
  | Sogou
  | ZiGuang

getDoubleSuffix :: DoublePinyinSchema -> String
getDoubleSuffix =
  \case
    Natural -> ""
    Flypy -> "_flypy"
    ABC -> "_abc"
    MSPY -> "_mspy"
    Sogou -> "_sogou"
    ZiGuang -> "_ziguang"

getDoubleSuffix' :: DoublePinyinSchema -> String
getDoubleSuffix' =
  \case
    Natural -> "_double_pinyin"
    Flypy -> "_flypy"
    ABC -> "_abc"
    MSPY -> "_mspy"
    Sogou -> "_sogou"
    ZiGuang -> "_ziguang"

getDoubleMeltEngSchema :: DoublePinyinSchema -> RimeComponent
getDoubleMeltEngSchema doublePinyinSchema =
  let
    suffix = getDoubleSuffix doublePinyinSchema
    suffix' = getDoubleSuffix' doublePinyinSchema
   in
    Tr.Node
      ( "melt_eng_double_pinyin" <> suffix
      ,
        [ RimeTransformationProduce
            ("melt_eng_double_pinyin" <> suffix <> ".schema.yaml")
            ( printf
                [r|
schema:
  schema_id: melt_eng_double_pinyin%s
__include: melt_eng.schema:/
__patch:
  "speller/algebra":
    __include: melt_eng.schema:/algebra%s
  "translator/prism": melt_eng_double_pinyin%s
|]
                suffix
                suffix'
                suffix
            )
        ]
      )
      [meltEngSchema]

getDoubleRadicalPinyinSchema :: DoublePinyinSchema -> RimeComponent
getDoubleRadicalPinyinSchema doublePinyinSchema =
  let
    suffix = getDoubleSuffix doublePinyinSchema
    suffix' = getDoubleSuffix' doublePinyinSchema
   in
    Tr.Node
      ( "radical_pinyin_double_pinyin" <> suffix
      ,
        [ RimeTransformationProduce
            ("radical_pinyin_double_pinyin" <> suffix <> ".schema.yaml")
            ( printf
                [r|
schema:
  schema_id: radical_pinyin_double_pinyin%s
__include: radical_pinyin.schema:/
__patch:
  "speller/algebra":
    __include: radical_pinyin.schema:/algebra%s
  "translator/prism": radical_pinyin_double_pinyin%s
|]
                suffix
                suffix'
                suffix
            )
        ]
      )
      [radicalPinyinSchema]

getDoublePinyinSchema :: DoublePinyinSchema -> RimeComponent
getDoublePinyinSchema doublePinyinSchema =
  let
    suffix = getDoubleSuffix doublePinyinSchema
    suffix' = getDoubleSuffix' doublePinyinSchema
   in
    Tr.Node
      ( "double-pinyin" <> replace "_" "-" suffix
      ,
        [ RimeTransformationApply
            ("double_pinyin" <> suffix <> ".schema.yaml")
            ("rime_ice_double_pinyin" <> suffix <> ".schema.yaml")
            ( -- Since we want to keep comments in yaml file, we can't deserialize original file then serialize it back. So we can only use `replace ` to modify the content.
              replace
                "@radical_pinyin"
                ("@radical_pinyin_double_pinyin" <> suffix)
                . replace
                  (printf "radical_lookup:\n  tag: radical_lookup\n  dictionary: radical_pinyin")
                  (printf "radical_lookup:\n  tag: radical_lookup\n  dictionary: radical_pinyin\n  prism: %s" ("radical_pinyin_double_pinyin" <> suffix))
                . replace "- radical_pinyin" ("- radical_pinyin_double_pinyin" <> suffix)
                . replace
                  (printf "melt_eng:\n  dictionary: melt_eng")
                  (printf "melt_eng:\n  dictionary: melt_eng\n  prism: %s" ("melt_eng_double_pinyin" <> suffix))
                . replace "- melt_eng" ("- melt_eng_double_pinyin" <> suffix)
                . replace ("double_pinyin" <> suffix) ("rime_ice_double_pinyin" <> suffix)
            )
        , RimeTransformationProduce ("custom_phrase_double_pinyin" <> suffix <> ".txt") ""
        , RimeTransformationIdentity ("en_dicts/cn_en" <> suffix' <> ".txt")
        ]
      )
      [ rimeIceDoubleDefaultPatch
      , rimeIcePinyinSchema
      , getDoubleMeltEngSchema doublePinyinSchema
      , getDoubleRadicalPinyinSchema doublePinyinSchema
      , vCapSymbols
      ]

allComponent :: RimeComponent
allComponent =
  Tr.Node
    ("all", [])
    ( [rimeIcePinyinSchema]
        <> map
          getDoublePinyinSchema
          [ Natural
          , Flypy
          , ABC
          , MSPY
          , Sogou
          , ZiGuang
          ]
    )
