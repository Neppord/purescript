module Test.TestCst where

import Prelude

import Data.Array (mapMaybe, reverse, (:))
import Data.Foldable (foldl, for_, sum)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Common (replace, split)
import Data.String.Pattern (Pattern(..), Replacement(Replacement))
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.Path (FilePath)
import PureScript.CST.TokenStream (TokenStep(..), step)
import PureScript.CST.Types (Token(TokLayoutEnd, TokLayoutSep, TokLayoutStart))
import Test.Spec (Spec, describe, it)
import Test.TestUtils (getTestFiles, goldenVsString)
import Node.FS.Aff (readTextFile) as Aff
import Data.Array (take) as Array
import PureScript.CST.Lexer (lex) as Lexer
import Data.String.CodeUnits (length, splitAt) as String
import Data.String.Common (split) as String

spec :: Spec Unit
spec = do
  layoutSpec

-- literalsSpec

layoutSpec :: Spec Unit
layoutSpec =
  describe "Layout golden tests" do
    it "layout" do
      (pursFiles :: Array FilePath) <- getTestFiles "layout"
      for_ pursFiles $ \file -> do
        actual <- runLexer file
        let golden_filename = replace (Pattern ".purs") (Replacement ".out") file
        goldenVsString golden_filename actual

runLexer :: FilePath -> Aff String
runLexer file = do
  src <- Aff.readTextFile UTF8 file
  let
    lines = String.split (Pattern "\n") src
    line_lengths = lines <#> String.length
    tokenStream = Lexer.lex src
    tokens stream = case step stream of
      TokenEOF _ _ -> []
      TokenError _ _ _ _ -> []
      TokenCons token _ nextStream _ -> token : tokens nextStream
    layouts = tokenStream
      # tokens
      # mapMaybe \{ value, range: { end: { line, column } } } ->
          let
            pos = (line_lengths # Array.take line # sum) + column + line
          in
            case value of
              TokLayoutStart _ -> Just { value: "{", pos }
              TokLayoutSep _ -> Just { value: ";", pos }
              TokLayoutEnd _ -> Just { value: "}", pos }
              _ -> Nothing
    insert s { value, pos } =
      String.splitAt pos s # \{ before, after } -> before <> value <> after

  pure $ ((foldl insert src $ reverse layouts) <> "\n<eof>")

{-
literalsSpec :: Spec Unit
literalsSpec = describe "Literals" do
  testProperty "Integer" $
    checkTok checkReadNum
      case _ of
        TokInt _ a -> Just a
        _ -> Nothing
      >>> unInt
  testProperty "Hex" $
    checkTok checkReadNum
      case _ of
        TokInt _ a -> Just a
        _ -> Nothing
      >>> unHex
  testProperty "Number" $
    checkTok checkReadNum
      case _ of
        TokNumber _ a -> Just a
        _ -> Nothing
      >>> unFloat
  testProperty "Exponent" $
    checkTok checkReadNum
      case _ of
        TokNumber _ a -> Just a
        _ -> Nothing
      >>> unExponent

  testProperty "Integer (round trip)" $ roundTripTok >>> unInt
  testProperty "Hex (round trip)" $ roundTripTok >>> unHex
  testProperty "Number (round trip)" $ roundTripTok >>> unFloat
  testProperty "Exponent (round trip)" $ roundTripTok >>> unExponent
  testProperty "Char (round trip)" $ roundTripTok >>> unChar
  testProperty "String (round trip)" $ roundTripTok >>> unString
  testProperty "Raw String (round trip)" $ roundTripTok >>> unRawString

  where
  testProperty name test = specify name (property test)

readTok' :: String -> Text -> Gen SourceToken
readTok' failMsg t = case CST.lex t of
  Right tok : _ ->
    pure tok
  Left (_ /\ err) : _ ->
    error $ failMsg <> ": " <> CST.prettyPrintError err
  [] ->
    error "Empty token stream"

readTok :: Text -> Gen SourceToken
readTok = readTok' "Failed to parse"

checkTok
  :: (Text -> a -> Gen Bool)
  -> (Token -> Maybe a)
  -> Text
  -> Gen Bool
checkTok p f t = do
  SourceToken _ tok <- readTok t
  case f tok of
    Just a -> p t a
    Nothing -> error $ "Failed to lex correctly: " <> show tok

roundTripTok :: Text -> Gen Bool
roundTripTok t = do
  tok <- readTok t
  let t' = CST.printTokens [ tok ]
  tok' <- readTok' "Failed to re-parse" t'
  pure $ tok == tok'

checkReadNum :: Eq a => Read a => Text -> a -> Gen Bool
checkReadNum t a = do
  let
    chs = case Text.unpack $ Text.replace ".e" ".0e" $ Text.replace "_" "" t of
      chs' | last chs' == '.' -> chs' <> "0"
      chs' -> chs'
  case (_ == a) <$> readMaybe chs of
    Just a' -> pure a'
    Nothing -> error "Failed to `read`"

newtype PSSourceInt = PSSourceInt { unInt :: Text }

instance Arbitrary PSSourceInt where
  arbitrary = resize 16 genInt

newtype PSSourceFloat = PSSourceFloat { unFloat :: Text }

instance Arbitrary PSSourceFloat where
  arbitrary = resize 16 genFloat

newtype PSSourceExponent = PSSourceExponent { unExponent :: Text }

instance Arbitrary PSSourceExponent where
  arbitrary = PSSourceExponent <$> do
    floatPart <- unFloat <$> resize 5 genFloat
    signPart <- fromMaybe "" <$> elements [ Just "+", Just "-", Nothing ]
    expPart <- unInt <$> resize 1 genInt
    pure $ floatPart <> "e" <> signPart <> expPart

newtype PSSourceHex = PSSourceHex { unHex :: Text }

instance Arbitrary PSSourceHex where
  arbitrary = resize 16 genHex

newtype PSSourceChar = PSSourceChar { unChar :: Text }

instance Arbitrary PSSourceChar where
  arbitrary = genChar

newtype PSSourceString = PSSourceString { unString :: Text }

instance Arbitrary PSSourceString where
  arbitrary = resize 256 genString

newtype PSSourceRawString = PSSourceRawString { unRawString :: Text }

instance Arbitrary PSSourceRawString where
  arbitrary = resize 256 genRawString

genInt :: Gen PSSourceInt
genInt = PSSourceInt <$> do
  (:) <$> nonZeroChar
    <*> listOf numChar

genFloat :: Gen PSSourceFloat
genFloat = PSSourceFloat <$> do
  intPart <- unInt <$> genInt
  floatPart <- listOf1 numChar
  pure $ intPart <> "." <> floatPart

genHex :: Gen PSSourceHex
genHex = PSSourceHex <$> do
  nums <- listOf1 hexDigit
  pure $ "0x" <> nums

genChar :: Gen PSSourceChar
genChar = PSSourceChar <$> do
  ch <- resize 0xFFFF arbitrarySizedNatural >>= (genStringChar '\''.toEnum)
  pure $ "'" <> ch <> "'"

genString :: Gen PSSourceString
genString = PSSourceString <$> do
  chs <- listOf $ arbitraryUnicodeChar >>= genStringChar '"'
  pure $ "\"" <> Text.concat chs <> "\""

genStringChar :: Char -> Char -> Gen Text
genStringChar delimiter ch = frequency
  [ 1 /\ genCharEscape
  , 10 /\
        if ch `elem` [ delimiter, '\n', '\r', '\\' ] then discard
        else pure $ Text.singleton ch
  ]

genRawString :: Gen PSSourceRawString
genRawString = PSSourceRawString <$> do
  chs <- listOf arbitraryUnicodeChar
  let
    k1 acc qs cs = do
      let (cs' /\ q) = span (_ /= '"') cs
      k2 (acc <> cs') qs q
    k2 acc qs [] = acc <> qs
    k2 acc qs cs = do
      let (q /\ cs') = span (_ == '"') cs
      k1 (acc <> take 2 q) (qs <> drop 2 q) cs'
    chs' = k1 [] [] chs
  when (all (_ == '"') chs') discard
  pure $ "\"\"\"" <> chs' <> "\"\"\""

genCharEscape :: Gen Text
genCharEscape = oneof
  [ pure "\\t"
  , pure "\\r"
  , pure "\\n"
  , pure "\\\""
  , pure "\\'"
  , pure "\\\\"
  , do
      chs <- resize 4 $ listOf1 hexDigit
      pure $ "\\x" <> chs
  ]

numChar :: Gen Char
numChar = elements "0123456789_"

nonZeroChar :: Gen Char
nonZeroChar = elements "123456789"

hexDigit :: Gen Char
hexDigit = elements $ [ 'a' .. 'f' ] <> [ 'A' .. 'F' ] <> [ '0' .. '9' ]
-}