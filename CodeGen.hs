{-# LANGUAGE TemplateHaskell #-}
-- | A code generation quasi-quoter. Everything is taken as literal text, with ~var~ variable interpolation, and ~~ is completely ignored.
module CodeGen (codegen) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.ParserCombinators.Parsec

codegen :: QuasiQuoter
codegen = QuasiQuoter codegen' $ error "codegen cannot be a pattern"

data Token = VarToken String | LitToken String | EmptyToken

codegen' :: String -> Q Exp
codegen' s' = do
    let s = killFirstBlank s'
    case parse (many parseToken) s s of
        Left e -> error $ show e
        Right tokens -> do
            let tokens' = map toExp tokens
            concat' <- [|concat|]
            return $ concat' `AppE` ListE tokens'
  where
    killFirstBlank ('\n':x) = x
    killFirstBlank ('\r':'\n':x) = x
    killFirstBlank x = x

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
