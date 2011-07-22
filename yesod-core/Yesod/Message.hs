{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Yesod.Message
    ( mkMessage
    , RenderMessage (..)
    , ToMessage (..)
    ) where

import Language.Haskell.TH.Syntax
import Data.Text (Text, pack, unpack)
import System.Directory
import Data.Maybe (catMaybes)
import Data.List (isSuffixOf, sortBy, foldl')
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8)
import Data.Char (isSpace, toLower, toUpper)
import Data.Ord (comparing)
import Text.Shakespeare (Deref (..), Ident (..), parseHash, derefToExp)
import Text.ParserCombinators.Parsec (parse, many, eof, many1, noneOf, (<|>))
import Control.Arrow ((***))
import Data.Monoid (mempty, mappend)

class ToMessage a where
    toMessage :: a -> Text
instance ToMessage Text where
    toMessage = id
instance ToMessage String where
    toMessage = Data.Text.pack

class RenderMessage master message where
    renderMessage :: master
                  -> [Text] -- ^ languages
                  -> message
                  -> Text

instance RenderMessage master Text where
    renderMessage _ _ = id

type Lang = Text

mkMessage :: String
          -> FilePath
          -> Lang
          -> Q [Dec]
mkMessage dt folder lang = do
    files <- qRunIO $ getDirectoryContents folder
    contents <- qRunIO $ fmap catMaybes $ mapM (loadLang folder) files
    sdef <-
        case lookup lang contents of
            Nothing -> error $ "Did not find main language file: " ++ unpack lang
            Just def -> toSDefs def
    mapM_ (checkDef sdef) $ map snd contents
    let dt' = ConT $ mkName dt
    let mname = mkName $ dt ++ "Message"
    c1 <- fmap concat $ mapM (toClauses dt) contents
    c2 <- mapM (sToClause dt) sdef
    c3 <- defClause
    return
        [ DataD [] mname [] (map (toCon dt) sdef) []
        , InstanceD
            []
            (ConT ''RenderMessage `AppT` dt' `AppT` ConT mname)
            [ FunD (mkName "renderMessage") $ c1 ++ c2 ++ [c3]
            ]
        ]

toClauses :: String -> (Lang, [Def]) -> Q [Clause]
toClauses dt (lang, defs) =
    mapM go defs
  where
    go def = do
        a <- newName "lang"
        (pat, bod) <- mkBody dt (constr def) (map fst $ vars def) (content def)
        guard <- fmap NormalG [|$(return $ VarE a) == pack $(lift $ unpack lang)|]
        return $ Clause
            [WildP, ConP (mkName ":") [VarP a, WildP], pat]
            (GuardedB [(guard, bod)])
            []

mkBody :: String -- ^ datatype
       -> String -- ^ constructor
       -> [String] -- ^ variable names
       -> [Content]
       -> Q (Pat, Exp)
mkBody dt cs vs ct = do
    vp <- mapM go vs
    let pat = RecP (mkName $ "Msg" ++ cs) (map (varName dt *** VarP) vp)
    let ct' = map (fixVars vp) ct
    pack' <- [|Data.Text.pack|]
    tomsg <- [|toMessage|]
    let ct'' = map (toH pack' tomsg) ct'
    mapp <- [|mappend|]
    let app a b = InfixE (Just a) mapp (Just b)
    e <-
        case ct'' of
            [] -> [|mempty|]
            [x] -> return x
            (x:xs) -> return $ foldl' app x xs
    return (pat, e)
  where
    toH pack' _ (Raw s) = pack' `AppE` SigE (LitE (StringL s)) (ConT ''String)
    toH _ tomsg (Var d) = tomsg `AppE` derefToExp [] d
    go x = do
        let y = mkName $ '_' : x
        return (x, y)
    fixVars vp (Var d) = Var $ fixDeref vp d
    fixVars _ (Raw s) = Raw s
    fixDeref vp (DerefIdent (Ident i)) = DerefIdent $ Ident $ fixIdent vp i
    fixDeref vp (DerefBranch a b) = DerefBranch (fixDeref vp a) (fixDeref vp b)
    fixDeref _ d = d
    fixIdent vp i =
        case lookup i vp of
            Nothing -> i
            Just y -> nameBase y

sToClause :: String -> SDef -> Q Clause
sToClause dt sdef = do
    (pat, bod) <- mkBody dt (sconstr sdef) (map fst $ svars sdef) (scontent sdef)
    return $ Clause
        [WildP, ConP (mkName "[]") [], pat]
        (NormalB bod)
        []

defClause :: Q Clause
defClause = do
    a <- newName "sub"
    c <- newName "langs"
    d <- newName "msg"
    rm <- [|renderMessage|]
    return $ Clause
        [VarP a, ConP (mkName ":") [WildP, VarP c], VarP d]
        (NormalB $ rm `AppE` VarE a `AppE` VarE c `AppE` VarE d)
        []

toCon :: String -> SDef -> Con
toCon dt (SDef c vs _) =
    RecC (mkName $ "Msg" ++ c) $ map go vs
  where
    go (n, t) = (varName dt n, NotStrict, ConT $ mkName t)

varName :: String -> String -> Name
varName a y =
    mkName $ concat [lower a, "Message", upper y]
  where
    lower (x:xs) = toLower x : xs
    lower [] = []
    upper (x:xs) = toUpper x : xs
    upper [] = []

checkDef :: [SDef] -> [Def] -> Q ()
checkDef x y =
    go (sortBy (comparing sconstr) x) (sortBy (comparing constr) y)
  where
    go _ [] = return ()
    go [] (b:_) = error $ "Extra message constructor: " ++ constr b
    go (a:as) (b:bs)
        | sconstr a < constr b = go as (b:bs)
        | sconstr a > constr b = error $ "Extra message constructor: " ++ constr b
        | otherwise = do
            go' (svars a) (vars b)
            go as bs
    go' ((an, at):as) ((bn, mbt):bs)
        | an /= bn = error "Mismatched variable names"
        | otherwise =
            case mbt of
                Nothing -> go' as bs
                Just bt
                    | at == bt -> go' as bs
                    | otherwise -> error "Mismatched variable types"
    go' [] [] = return ()
    go' _ _ = error "Mistmached variable count"

toSDefs :: [Def] -> Q [SDef]
toSDefs = mapM toSDef

toSDef :: Def -> Q SDef
toSDef d = do
    vars' <- mapM go $ vars d
    return $ SDef (constr d) vars' (content d)
  where
    go (a, Just b) = return (a, b)
    go (a, Nothing) = error $ "Main language missing type for " ++ show (constr d, a)

data SDef = SDef
    { sconstr :: String
    , svars :: [(String, String)]
    , scontent :: [Content]
    }

data Def = Def
    { constr :: String
    , vars :: [(String, Maybe String)]
    , content :: [Content]
    }

loadLang :: FilePath -> FilePath -> IO (Maybe (Lang, [Def]))
loadLang folder file = do
    let file' = folder ++ '/' : file
    e <- doesFileExist file'
    if e && ".msg" `isSuffixOf` file
        then do
            let lang = pack $ reverse $ drop 4 $ reverse file
            bs <- S.readFile file'
            let s = unpack $ decodeUtf8 bs
            defs <- fmap catMaybes $ mapM parseDef $ lines s
            return $ Just (lang, defs)
        else return Nothing

parseDef :: String -> IO (Maybe Def)
parseDef "" = return Nothing
parseDef ('#':_) = return Nothing
parseDef s =
    case end of
        ':':end' -> do
            content' <- fmap compress $ parseContent $ dropWhile isSpace end'
            case words begin of
                [] -> error $ "Missing constructor: " ++ s
                (w:ws) -> return $ Just Def
                            { constr = w
                            , vars = map parseVar ws
                            , content = content'
                            }
        _ -> error $ "Missing colon: " ++ s
  where
    (begin, end) = break (== ':') s

data Content = Var Deref | Raw String

compress :: [Content] -> [Content]
compress [] = []
compress (Raw a:Raw b:rest) = compress $ Raw (a ++ b) : rest
compress (x:y) = x : compress y

parseContent :: String -> IO [Content]
parseContent s =
    either (error . show) return $ parse go s s
  where
    go = do
        x <- many go'
        eof
        return x
    go' = (Raw `fmap` many1 (noneOf "#")) <|> (fmap (either Raw Var) parseHash)

parseVar :: String -> (String, Maybe String)
parseVar s =
    case break (== '@') s of
        (x, '@':y) -> (x, Just y)
        _ -> (s, Nothing)
