{-# LANGUAGE OverloadedStrings #-}

module Network.WebBridge.JSAPI where

import Network.WebBridge.Internal

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

type FuncName = Text

nativeFunc0 :: FuncName -> ClientJS a
nativeFunc0 n = emitJS $ JSSource n <++> JSSource "()"

nativeFunc1 :: FuncName -> (JS a) -> ClientJS b
nativeFunc1 n a = emitJS $ JSSource n <++> (parens . toJSSource) a

nativeFunc2 :: FuncName -> (JS a) -> (JS b) -> ClientJS c
nativeFunc2 n a b = emitJS $ JSSource n <++> parens (toJSSource a <.> toJSSource b)

nativeFunc3 :: FuncName -> (JS a) -> (JS b) -> (JS c) -> ClientJS d
nativeFunc3 n a b c = emitJS $ JSSource n <++> parens (toJSSource a <.> toJSSource b <.> toJSSource c)


alert :: JS String -> ClientJS ()
alert = nativeFunc1 "alert"

prompt :: JS String -> ClientJS String
prompt = nativeFunc1 "prompt"
