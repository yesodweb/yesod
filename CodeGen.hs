{-# LANGUAGE TemplateHaskell #-}
-- | A code generation template haskell. Everything is taken as literal text,
-- with ~var~ variable interpolation.
module CodeGen (codegen) where

import Language.Haskell.TH.Syntax
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

data Token = VarToken String | LitToken String | EmptyToken

codegen :: FilePath -> Q Exp
codegen fp = do
    s' <- qRunIO $ L.readFile $ "scaffold/" ++ fp ++ ".cg"
    let s = init $ LT.unpack $ LT.decodeUtf8 s'
    case parse (many parseToken) s s of
        Left e -> error $ show e
        Right tokens' -> do
            let tokens'' = map toExp tokens'
            concat' <- [|concat|]
            return $ concat' `AppE` ListE tokens''

toExp :: Token -> Exp
toExp (LitToken s) = LitE $ StringL s
toExp (VarToken s) = VarE $ mkName s
toExp EmptyToken = LitE $ StringL ""

parseToken :: Parser Token
parseToken =
    parseVar <|> parseLit
  where
    parseVar = do
        _ <- char '~'
        s <- many alphaNum
        _ <- char '~'
        return $ if null s then EmptyToken else VarToken s
    parseLit = do
        s <- many1 $ noneOf "~"
        return $ LitToken s
