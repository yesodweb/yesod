{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Libyaml as Y
import qualified Data.ByteString.Char8 as B8

import Test.HUnit hiding (Test, path)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import System.Directory
import Control.Monad
import Control.Exception (try, SomeException)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()

import qualified Data.Yaml as D
import Data.Yaml (object, array)
import Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

main :: IO ()
main = hspecX $ do
    describe "streaming" $ do
        it "count scalars with anchor" caseCountScalarsWithAnchor
        it "count sequences with anchor" caseCountSequencesWithAnchor
        it "count mappings with anchor" caseCountMappingsWithAnchor
        it "count aliases" caseCountAliases
        it "count scalars" caseCountScalars
        it "largest string" caseLargestString
        it "encode/decode" caseEncodeDecode
        it "encode/decode file" caseEncodeDecodeFile
        it "interleaved encode/decode" caseInterleave
        it "decode invalid document (without segfault)" caseDecodeInvalidDocument
    describe "Data.Yaml" $ do
        it "encode/decode" caseEncodeDecodeData
        it "encode/decode file" caseEncodeDecodeFileData
        it "encode/decode strings" caseEncodeDecodeStrings
        it "decode invalid file" caseDecodeInvalid
        it "processes datatypes" caseDataTypes
    describe "Data.Yaml aliases" $ do
        it "simple scalar alias" caseSimpleScalarAlias
        it "simple sequence alias" caseSimpleSequenceAlias
        it "simple mapping alias" caseSimpleMappingAlias
        it "mapping alias before anchor" caseMappingAliasBeforeAnchor
        it "mapping alias inside anchor" caseMappingAliasInsideAnchor
        it "scalar alias overriding" caseScalarAliasOverriding
    describe "Data.Yaml merge keys" $ do
        it "test uniqueness of keys" caseAllKeysShouldBeUnique
        it "test mapping merge" caseSimpleMappingMerge
        it "test sequence of mappings merging" caseMergeSequence

counter :: Monad m => (Y.Event -> Bool) -> C.Sink Y.Event m Int
counter pred' =
    CL.fold (\cnt e -> (if pred' e then 1 else 0) + cnt) 0

caseHelper :: String
           -> (Y.Event -> Bool)
           -> Int
           -> Assertion
caseHelper yamlString pred' expRes = do
    res <- C.runResourceT $ Y.decode (B8.pack yamlString) C.$$ counter pred'
    res @?= expRes

caseCountScalarsWithAnchor :: Assertion
caseCountScalarsWithAnchor =
    caseHelper yamlString isScalarA 1
  where
    yamlString = "foo:\n  - &anchor bin1\n  - bin2\n  - bin3"
    isScalarA (Y.EventScalar _ _ _ (Just _)) = True
    isScalarA _ = False

caseCountSequencesWithAnchor :: Assertion
caseCountSequencesWithAnchor =
    caseHelper yamlString isSequenceStartA 1
  where
    yamlString = "foo: &anchor\n  - bin1\n  - bin2\n  - bin3"
    isSequenceStartA (Y.EventSequenceStart (Just _)) = True
    isSequenceStartA _ = False

caseCountMappingsWithAnchor :: Assertion
caseCountMappingsWithAnchor =
    caseHelper yamlString isMappingA 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3"
    isMappingA (Y.EventMappingStart (Just _)) = True
    isMappingA _ = False

caseCountAliases :: Assertion
caseCountAliases =
    caseHelper yamlString isAlias 1
  where
    yamlString = "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3\nboo: *anchor"
    isAlias Y.EventAlias{} = True
    isAlias _ = False

caseCountScalars :: Assertion
caseCountScalars = do
    res <- C.runResourceT $ Y.decode yamlBS C.$$ CL.fold adder accum
    res @?= (7, 1, 2)
  where
    yamlString = "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
    yamlBS = B8.pack yamlString
    adder (s, l, m) (Y.EventScalar{})        = (s + 1, l, m)
    adder (s, l, m) (Y.EventSequenceStart{}) = (s, l + 1, m)
    adder (s, l, m) (Y.EventMappingStart{})  = (s, l, m + 1)
    adder a         _                      = a
    accum = (0, 0, 0) :: (Int, Int, Int)

caseLargestString :: Assertion
caseLargestString = do
    res <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.fold adder accum
    res @?= (length expected, expected)
    where
        expected = "this one is just a little bit bigger than the others"
        filePath = "test/largest-string.yaml"
        adder (i, s) (Y.EventScalar bs _ _ _) =
            let s' = B8.unpack bs
                i' = length s'
             in if i' > i then (i', s') else (i, s)
        adder acc _ = acc
        accum = (0, "no strings found")

newtype MyEvent = MyEvent Y.Event deriving Show
instance Eq MyEvent where
    (MyEvent (Y.EventScalar s t _ _)) == (MyEvent (Y.EventScalar s' t' _ _)) =
        s == s' && t == t'
    MyEvent e1 == MyEvent e2 = e1 == e2

caseEncodeDecode :: Assertion
caseEncodeDecode = do
    eList <- C.runResourceT $ Y.decode yamlBS C.$$ CL.consume
    bs <- C.runResourceT $ CL.sourceList eList C.$$ Y.encode
    eList2 <- C.runResourceT $ Y.decode bs C.$$ CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    yamlString = "foo: bar\nbaz:\n - bin1\n - bin2\n"
    yamlBS = B8.pack yamlString

removeFile' :: FilePath -> IO ()
removeFile' fp = do
    x <- doesFileExist fp
    when x $ removeFile fp

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = do
    removeFile' tmpPath
    eList <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.consume
    C.runResourceT $ CL.sourceList eList C.$$ Y.encodeFile tmpPath
    eList2 <- C.runResourceT $ Y.decodeFile filePath C.$$ CL.consume
    map MyEvent eList @=? map MyEvent eList2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"

caseInterleave :: Assertion
caseInterleave = do
    removeFile' tmpPath
    removeFile' tmpPath2
    () <- C.runResourceT $ Y.decodeFile filePath C.$$ Y.encodeFile tmpPath
    () <- C.runResourceT $ Y.decodeFile tmpPath C.$$ Y.encodeFile tmpPath2
    f1 <- readFile tmpPath
    f2 <- readFile tmpPath2
    f1 @=? f2
  where
    filePath = "test/largest-string.yaml"
    tmpPath = "tmp.yaml"
    tmpPath2 = "tmp2.yaml"

caseDecodeInvalidDocument :: Assertion
caseDecodeInvalidDocument = do
    x <- try $ C.runResourceT $ Y.decode yamlBS C.$$ CL.sinkNull
    case x of
        Left (_ :: SomeException) -> return ()
        Right y -> do
            putStrLn $ "bad return value: " ++ show y
            assertFailure "expected parsing exception, but got no errors"
  where
    yamlString = "  - foo\n  - baz\nbuz"
    yamlBS = B8.pack yamlString

mkScalar :: String -> D.Value
mkScalar = mkStrScalar

mkStrScalar :: String -> D.Value
mkStrScalar = D.String . T.pack

mappingKey :: D.Value-> String -> D.Value
mappingKey (D.Object m) k = (fromJust . M.lookup (T.pack k) $ m)
mappingKey _ _ = error "expected Object"

decodeYaml :: String -> Maybe D.Value
decodeYaml s = D.decode $ B8.pack s

sample :: D.Value
sample = array
    [ D.String "foo"
    , object
        [ ("bar1", D.String "bar2")
        ]
    ]

caseEncodeDecodeData :: Assertion
caseEncodeDecodeData = do
    let out = D.decode $ D.encode sample
    out @?= Just sample

caseEncodeDecodeFileData :: Assertion
caseEncodeDecodeFileData = do
    let fp = "tmp.yaml"
    D.encodeFile fp sample
    out <- D.decodeFile fp
    out @?= Just sample

caseEncodeDecodeStrings :: Assertion
caseEncodeDecodeStrings = do
    let out = D.decode $ D.encode sample
    out @?= Just sample

caseDecodeInvalid :: Assertion
caseDecodeInvalid = do
    let invalid = B8.pack "\tthis is 'not' valid :-)"
    Nothing @=? (D.decode invalid :: Maybe D.Value)

caseSimpleScalarAlias :: Assertion
caseSimpleScalarAlias = do
    let maybeRes = decodeYaml "- &anch foo\n- baz\n- *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= array [(mkScalar "foo"), (mkScalar "baz"), (mkScalar "foo")]

caseSimpleSequenceAlias :: Assertion
caseSimpleSequenceAlias = do
    let maybeRes = decodeYaml "seq: &anch\n  - foo\n  - baz\nseq2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= object [("seq", array [(mkScalar "foo"), (mkScalar "baz")]), ("seq2", array [(mkScalar "foo"), (mkScalar "baz")])]

caseSimpleMappingAlias :: Assertion
caseSimpleMappingAlias = do
    let maybeRes = decodeYaml "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= object [(T.pack "map", object [("key1", mkScalar "foo"), ("key2", (mkScalar "baz"))]), (T.pack "map2", object [("key1", (mkScalar "foo")), ("key2", mkScalar "baz")])]

caseMappingAliasBeforeAnchor :: Assertion
caseMappingAliasBeforeAnchor = do
    let res = decodeYaml "map: *anch\nmap2: &anch\n  key1: foo\n  key2: baz"
    isNothing res @? "decode should return Nothing due to unknown alias"

caseMappingAliasInsideAnchor :: Assertion
caseMappingAliasInsideAnchor = do
    let res = decodeYaml "map: &anch\n  key1: foo\n  key2: *anch"
    isNothing res @? "decode should return Nothing due to unknown alias"

caseScalarAliasOverriding :: Assertion
caseScalarAliasOverriding = do
    let maybeRes = decodeYaml "- &anch foo\n- baz\n- *anch\n- &anch boo\n- buz\n- *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= array [(mkScalar "foo"), (mkScalar "baz"), (mkScalar "foo"), (mkScalar "boo"), (mkScalar "buz"), (mkScalar "boo")]

caseAllKeysShouldBeUnique :: Assertion
caseAllKeysShouldBeUnique = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\nfoo1: buz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= (mkScalar "buz")

caseSimpleMappingMerge :: Assertion
caseSimpleMappingMerge = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\n<<:\n  foo1: buz\n  foo3: fuz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= (mkScalar "foo")
    mappingKey res "foo3" @?= (mkScalar "fuz")

caseMergeSequence :: Assertion
caseMergeSequence = do
    let maybeRes = decodeYaml "m1: &m1\n  k1: !!str 1\n  k2: !!str 2\nm2: &m2\n  k1: !!str 3\n  k3: !!str 4\nfoo1: foo\n<<: [ *m1, *m2 ]"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= (mkScalar "foo")
    mappingKey res "k1" @?= (D.Number 1)
    mappingKey res "k2" @?= (D.Number 2)
    mappingKey res "k3" @?= (D.Number 4)

caseDataTypes :: Assertion
caseDataTypes =
    D.decode (D.encode val) @?= Just val
  where
    val = object
        [ ("string", D.String "foo")
        , ("int", D.Number 5)
        , ("float", D.Number 4.3)
        , ("true", D.Bool True)
        , ("false", D.Bool False)
        , ("null", D.Null)
        ]
