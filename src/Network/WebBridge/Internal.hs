{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.WebBridge.Internal where

import Data.Aeson ((.=), (.:))
import Data.Monoid
import Data.String
import Data.Text.Lazy (Text)

import Control.Applicative
import Control.Monad.Operational
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as T
import qualified Data.Aeson as JSON

import Prelude hiding ((++))

(++) :: Monoid a => a -> a -> a
(++) = mappend

(<++>) :: JSSource a -> JSSource b -> JSSource c
JSSource a <++> JSSource b = JSSource $ a ++ b

-- | Raw JavaScript source code. Parametrized by a phantom type
--   which carries the type that the source will evaluate to.
newtype JSSource a = JSSource { unSrc :: Text } deriving (Monoid, Show)

-- | Primitive instructions for the client side monad
data ClientInstr a where
    EmitJS :: JSSource (JS a) -> ClientInstr (JS a)

-- | The Client monad encompasses code that is translated into
--   JavaScript and ran in the browser.
type Client = Program ClientInstr

-- | Primitive instructions for the server side monad
data ServerInstr a where
    LiftIO    :: IO a -> ServerInstr a
    EvalWait  :: FromClient (JS a) => Client (JS a) -> ServerInstr (ServerRep (JS a))
    EvalAsync :: Client (JS a) -> ServerInstr ()

newtype Server a = Server (Program ServerInstr a)
    deriving (Functor, Applicative, Monad)

-- | A value in JavaScript that can either be a literal or
--   an expression that results in a value when its evaluated.
data JS a where
    LitString :: Text -> JS String
    LitNull   :: JS ()
    BinOp     :: Oper op -> JS a -> JS b -> JS (OpResult op a b)
    Var       :: VarName -> JS a

type VarName = Text
type ClientJS a = Client (JS a)

-- | Operator at both constuctor and type level.
--   Could be implemented using GHC's DataKinds.
data Oper op where
    Add :: Oper Add
    Sub :: Oper Sub
    Mul :: Oper Mul
    Div :: Oper Div

data Add
data Sub
data Mul
data Div

-- | Result type mapping from different operators and operand types
type family OpResult op a b
type instance OpResult Add String String = String

instance IsString (JS String) where
    fromString = LitString . fromString

instance Monoid (JS String) where
    mempty  = LitString ""
    mappend = BinOp Add

-- | Lifts for Server monad

instance MonadIO Server where
    liftIO = Server . singleton . LiftIO

-- | Typeclass for JS-types that have a representative type in Haskell
class JSON.FromJSON (ServerRep a) => FromClient a where
    type ServerRep a

instance FromClient (JS String) where
    type ServerRep (JS String) = Text

-- | Type for requests from the server to client
data WebRequest where
    ReqEval  :: ClientJS a -> ReqId -> WebRequest
    ReqAsync :: ClientJS a -> WebRequest

data WebResponse where
    Response :: JSON.Value -> ReqId -> WebResponse

instance JSON.FromJSON WebResponse where
    parseJSON (JSON.Object obj) =
        Response <$> (obj .: "result") <*> (obj .: "reqId")

    parseJSON _ = fail "invalid response"

instance JSON.ToJSON WebRequest where
    toJSON req = case req of
        ReqEval c rqId -> JSON.object $
            [ "method" .= JSON.String "eval"
            , "code"   .= c
            , "reqId"  .= rqId
            ]

        ReqAsync c -> JSON.object $
            [ "method" .= JSON.String "async"
            , "code"   .= c
            ]

instance JSON.ToJSON (ClientJS a) where
    toJSON = JSON.toJSON . unSrc . clientSource 0

type ReqId = Int
type VarId = Int

toJSSource :: JS a -> JSSource (JS a)
toJSSource js = case js of
    LitString s -> JSSource $ T.pack $ show s
    LitNull     -> JSSource "null"
    Var n       -> JSSource $ n
    BinOp o a b -> toJSSource a <++> op <++> toJSSource b where
        op = JSSource $ case o of
            Add -> "+"
            Sub -> "-"
            Mul -> "*"
            Div -> "/"

clientSource :: VarId -> ClientJS a -> JSSource (JS a)
clientSource initId client = JSSource $ inClosure $ src ++ retRes where

    inClosure = ("(function(){" ++) . (++ "})();")
    retRes = "return " ++ (unSrc . toJSSource) res
    (src, res) = go LitNull 0 client

    go res varId prog = case view prog of
        Return a -> ("", a)
        EmitJS (JSSource src) :>>= cont ->
            let name = "x" ++ T.pack (show varId)
                assignment = "var " ++ name ++ " = " ++ src ++ ";"
                (rest, res') = go (Var name) (varId+1) . cont . Var $ name
            in (assignment ++ rest, res')

onClient :: FromClient (JS a) => ClientJS a -> Server (ServerRep (JS a))
onClient = Server . singleton . EvalWait

emitJS :: JSSource (JS a) -> ClientJS a
emitJS = singleton . EmitJS

parens :: JSSource a -> JSSource a
parens = (JSSource "(" ++) . (++ JSSource ")")

(<.>) :: JSSource a -> JSSource b -> JSSource c
JSSource a <.> JSSource b = JSSource $ a ++ "," ++ b
