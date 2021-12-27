{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Zexpr.Text.Parser
  ( parse
  , isSymChar
  -- * Re-exports
  , TokenError
  , MP.errorBundlePretty
  ) where

import Prelude hiding (round,significand,exponent,exp)

import Control.Monad (when,void)
import Control.Monad.State.Strict (State,get,put,evalState)
import Data.Char (isAlphaNum,isDigit,isHexDigit,chr,ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Symbol (Symbol,intern)
import Data.Text (Text)
import Data.Void (Void)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Zexpr (Atom(..), Zexpr(..), Combine(..))
import Numeric (readHex)

import qualified Data.Zexpr.Zexpr as Zexpr
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Text.Megaparsec as MP


parse :: FilePath -> Text -> Either TokenError [Zexpr]
parse fname inp = evalState (MP.runParserT go fname inp) emptyIndentState
  where go = parseFile -- DEBUG

type Parser a = MP.ParsecT Void Text (State IndentState) a
type TokenError = MP.ParseErrorBundle Text Void

withLocation :: Parser a -> Parser (Loc, a)
withLocation p = do
  pos0 <- MP.getSourcePos
  r <- p
  pos <- MP.getSourcePos
  pure (adaptPos pos0 pos, r)

adaptPos :: MP.SourcePos -> MP.SourcePos -> Loc
adaptPos pos0 pos = Loc
  { filename = MP.sourceName pos
  , fromLine = fromIntegral . MP.unPos $ MP.sourceLine pos0
  , fromCol = fromIntegral . MP.unPos $ MP.sourceColumn pos0
  , toLine = fromIntegral . MP.unPos $ MP.sourceLine pos
  , toCol = fromIntegral . MP.unPos $ MP.sourceColumn pos
  }


------------------------------------ Grammar ------------------------------------

{-
File
  ::= <nextline>?
   |  <nextline>? Zexpr (<nextline> Zexpr)*
-}
parseFile :: Parser [Zexpr]
parseFile = do
  _ <- MP.optional nextline
  es <- MP.optional parseZexpr >>= \case
    Nothing -> pure []
    Just e -> do
      es <- MP.many $ do
        nextline
        parseZexpr
      pure (e:es)
  void $ do
    void $ MP.many $ MP.try (MP.optional inlineSpace >> newline)
    void $ MP.optional inlineSpace
  MP.eof
  pure es

parseZexpr :: Parser Zexpr
parseZexpr = withLocation parseZexprs <&> \(loc, preEs) -> case preEs of
  e :| [] -> e
  e :| es -> ZCombo loc Round (e:es)

{-
Zexpr
  ::= '$'? LineExpr
   |  '@' '$'? LineExpr <nextline> Zexpr
   |  '@@' '$'? LineExpr (<nextline> Zexpr)* -- TODO
-}
parseZexprs :: Parser (NonEmpty Zexpr)
parseZexprs = MP.choice
  [ do
      void $ MP.single '@' >> MP.optional inlineSpace
      e1 <- parseDollar
      nextline
      e' <- parseZexpr
      pure $ case e1 of { (e:|es) -> e :| (es ++ [e']) }
  , parseDollar
  ]
  where
  parseDollar :: Parser (NonEmpty Zexpr)
  parseDollar = do
    dollar_m <- MP.optional $ do
      (loc, _) <- withLocation $ MP.single '$'
      _ <- MP.optional inlineSpace
      pure $ ZCombo loc Dollar ()
    es <- parseLine
    case dollar_m of
      Nothing -> pure es
      Just dollar -> pure (dollar :| NE.toList es)

{-
LineExpr
  ::= ChainExpr+
   |  ChainExpr+ ':' Zexpr
   |  ChainExpr+ ':' <indent> (<nextline> Zexpr)+ <dedent>
   |  ChainExpr+ <ws> '::' <ws> Zexpr
-}
parseLine :: Parser (NonEmpty Zexpr)
parseLine = do
  lPos0 <- MP.getSourcePos
  e <- parseChain
  es <- MP.many (MP.try $ inlineSpace >> parseChain)
  lLoc <- adaptPos lPos0 <$> MP.getSourcePos
  MP.choice
    [ do
        void $ MP.try $ inlineSpace >> MP.chunk "::" >> inlineSpace
        let eL = case es of { [] -> e ; _ -> ZCombo lLoc Round (e:es) }
        eR <- parseZexpr
        pure $ eL :| [eR]
    , do
        void $ MP.try $ MP.optional inlineSpace >> MP.single ':' >> indent
        eLines <- MP.some $ nextline >> parseZexpr
        dedent
        pure $ e :| es ++ eLines
    , do
        void $ MP.try $ MP.optional inlineSpace >> MP.single ':'
        inlineSpace
        eR <- parseZexpr
        pure $ e :| es ++ [eR]
    , pure (e :| es)
    ]

{-
ChainExpr
  ::= Sexpr (<!ws> ChainSuffix)*
ChainSuffix
  ::= ComboExpr
   |  '.' <!ws> Number
   |  <!ws> <string>
   |  '.' <!ws> <symbol>
   |  '.' SquareCombo
-}
parseChain :: Parser Zexpr
parseChain = do
  e0 <- parseSexpr
  suffixes <- MP.many $ MP.choice [ call, string, dotted ]
  pure $ foldl (&) e0 suffixes
  where
  call :: Parser (Zexpr -> Zexpr)
  call = do
    args <- parseCombo
    pure $ flip go args
    where
    go :: Zexpr -> Zexpr -> Zexpr
    go e0 nil@(ZCombo loc Round []) =
      let loc' = Zexpr.loc e0 <> loc
       in ZCombo loc' Round [e0, nil]
    go e0 (ZCombo loc Round es) =
      let loc' = Zexpr.loc e0 <> loc
       in ZCombo loc' Round (e0:es)
    go e0 (ZCombo loc Square (_, [])) =
      let loc' = Zexpr.loc e0 <> loc
          nil = ZCombo loc Round []
       in ZCombo loc' LensIndex (e0, nil)
    go e0 (ZCombo loc Square (_, [e])) =
      let loc' = Zexpr.loc e0 <> loc
       in ZCombo loc' LensIndex (e0, e)
    go e0 (ZCombo loc Square (_, es)) =
      let loc' = Zexpr.loc e0 <> loc
          e = ZCombo loc Round es
       in ZCombo loc' LensIndex (e0, e)
    go e0 arg@(ZCombo loc ConsDot _) =
      let loc' = Zexpr.loc e0 <> loc
       in ZCombo loc' Round [e0, arg]
    go _ (ZAtom _ _) = errorWithoutStackTrace "internal error: parseCombo returned atom in parseChain"
    go _ (ZCombo _ LensField _) = errorWithoutStackTrace "internal error: parseCombo returned LensField in parseChain"
    go _ (ZCombo _ LensIndex _) = errorWithoutStackTrace "internal error: parseCombo returned LensIndex in parseChain"
    go _ (ZCombo _ FloatLit _) = errorWithoutStackTrace "internal error: parseCombo returned FloatLit in parseChain"
    go _ (ZCombo _ MakeInt _) = errorWithoutStackTrace "internal error: parseCombo returned MakeInt in parseChain"
    go _ (ZCombo _ MakeFloat _) = errorWithoutStackTrace "internal error: parseCombo returned MakeFloat in parseChain"
    go _ (ZCombo _ MakeStr _) = errorWithoutStackTrace "internal error: parseCombo returned MakeStr in parseChain"
    go _ (ZCombo _ MakeList _) = errorWithoutStackTrace "internal error: parseCombo returned MakeList in parseChain"
    go _ (ZCombo _ Dollar _) = errorWithoutStackTrace "internal error: parseCombo returned Dollar in parseChain"
    go _ (ZCombo _ Tick _) = errorWithoutStackTrace "internal error: parseCombo returned Tick in parseChain"
    go _ (ZCombo _ Backtick _) = errorWithoutStackTrace "internal error: parseCombo returned Backtick in parseChain"
    go _ (ZCombo _ Comma _) = errorWithoutStackTrace "internal error: parseCombo returned Comma in parseChain"
    go _ (ZCombo _ CommaAt _) = errorWithoutStackTrace "internal error: parseCombo returned CommaAt in parseChain"
    go _ (ZCombo _ QualName _) = errorWithoutStackTrace "internal error: parseCombo returned QualName in parseChain"
  string = do
    (loc, str) <- withLocation dqStr
    pure $ \e0 ->
      let loc' = Zexpr.loc e0 <> loc
       in ZCombo loc' MakeStr (e0, loc, str)
  dotted = do
    void $ MP.single '.'
    MP.choice
      [ do
          (xLoc, x) <- withLocation $ plainSym MP.<|> dqSym
          pure $ \e0 -> ZCombo (Zexpr.loc e0 <> xLoc) LensField (e0, xLoc, x)
      , parseNum <&> \case
          ZAtom nLoc (Int n) -> \e0 ->
            ZCombo (Zexpr.loc e0 <> nLoc) MakeInt (e0, nLoc, n)
          ZCombo nLoc FloatLit n -> \e0 ->
            ZCombo (Zexpr.loc e0 <> nLoc) MakeFloat (e0, nLoc, n)
          _ -> errorWithoutStackTrace "internal error: got non-number when parsing dotted"
      , parseSquare <&> \case
          ZCombo sqLoc Square (_, es) -> \e0 ->
            ZCombo (Zexpr.loc e0 <> sqLoc) MakeList (e0, sqLoc, es)
          _ -> errorWithoutStackTrace "internal error: got non-number when parsing dotted"
      ]

    --     [ Right <$> plainSym
    --     , Right <$> dqSym
    --     , Left <$> parseNum
    --     ]
    -- pure $ case suffix of
    --   Right f -> \e0 ->
    --     let loc' = Zexpr.loc e0 <> loc
    --      in ZCombo loc' LensField (e0, loc, f)
    --   Left (ZAtom _ (Int n)) -> \e0 ->
    --     let loc' = Zexpr.loc e0 <> loc
    --      in ZCombo loc' MakeInt (e0, loc, n)
    --   Left (ZCombo _ FloatLit repr) -> \e0 ->
    --     let loc' = Zexpr.loc e0 <> loc
    --      in ZCombo loc' MakeFloat (e0, loc, repr)
    --   Left _ -> errorWithoutStackTrace "internal error: got non-number when parsing dotted"

{-
Sexpr
  ::= AtomExpr
   |  ComboExpr
   |  Quote <!ws> ChainExpr
Quote
  ::= '\''
   | '`'
   | ','
   | ',@'
-}
parseSexpr :: Parser Zexpr
parseSexpr = MP.choice
  [ parseAtom
  , parseCombo
  , do
      pos0 <- MP.getSourcePos
      (qLoc, quoteful) <- parseQuotey
      e <- parseChain
      pos <- MP.getSourcePos
      let loc = adaptPos pos0 pos
      pure $ ZCombo loc quoteful (qLoc, e)
  ]
  where
  parseQuotey :: Parser (Loc, Combine (Loc, Zexpr))
  parseQuotey = withLocation $ MP.choice
    [ MP.single '\'' >> pure Tick
    , MP.single '`' >> pure Backtick
    , MP.chunk ",@" >> pure CommaAt -- WARNING must come before comma
    , MP.single ',' >> pure Comma
    ]

{-
ComboExpr ::= RoundCombo | SquareCombo | CurlyCombo
RoundCombo
  ::= '(' ')'
   |  '(' Zexpr (',' <ws> Zexpr)* ')'
   |  '(' Zexpr <ws> '.' <ws> Zexpr ')'
SquareCombo
  ::= '[' ']'
   |  '[' Zexpr (',' <ws> Zexpr)* ']'
CurlyCombo
  ::= '{' ChainExpr* '}'
-}
parseCombo :: Parser Zexpr
parseCombo = MP.choice
  [ parseRound
  , parseSquare
  , withLocation parseCurly <&> \(loc, es) -> case es of
      [] -> ZCombo loc Round []
      [e] -> ZCombo loc Round [e]
      (eL : eF : esR) -> ZCombo loc Round (eF : eL : esR)
  ]

parseRound :: Parser Zexpr
parseRound = do
  pos0 <- MP.getSourcePos
  _ <- openRound >> MP.optional inlineSpace
  let end = MP.try (MP.optional inlineSpace >> closeRound)
  MP.choice
    [ do
        end
        loc <- adaptPos pos0 <$> MP.getSourcePos
        pure $ ZCombo loc Round []
    , do
        (loc0, es) <- withLocation (NE.toList <$> parseZexprs)
        MP.choice
          [ do
              (dotLoc, _) <- withLocation consDot
              e' <- parseZexpr
              end
              loc <- adaptPos pos0 <$> MP.getSourcePos
              pure $ ZCombo loc ConsDot (es, dotLoc, e')
          , do
              let e0 = case es of { [it] -> it ; _ -> ZCombo loc0 Round es }
              es' <- MP.some (comma >> parseZexpr)
              end
              loc <- adaptPos pos0 <$> MP.getSourcePos
              pure $ ZCombo loc Round (e0:es')
          , do
              end
              loc <- adaptPos pos0 <$> MP.getSourcePos
              pure $ ZCombo loc Round es
          ]
    ]

parseSquare :: Parser Zexpr
parseSquare = do
  pos0 <- MP.getSourcePos
  (sqLoc, _) <- withLocation openSquare <* MP.optional inlineSpace
  let end = MP.try (MP.optional inlineSpace >> closeSquare)
  MP.choice
    [ do
        end
        loc <- adaptPos pos0 <$> MP.getSourcePos
        pure $ ZCombo loc Square (sqLoc, [])
    , do
        (loc0, es) <- withLocation (NE.toList <$> parseZexprs)
        MP.choice
          [ do
            let e0 = case es of { [it] -> it ; _ -> ZCombo loc0 Square (sqLoc, es) }
            es' <- MP.some (comma >> parseZexpr)
            end
            loc <- adaptPos pos0 <$> MP.getSourcePos
            pure $ ZCombo loc Square (sqLoc, e0:es')
          , do
              end
              loc <- adaptPos pos0 <$> MP.getSourcePos
              pure $ ZCombo loc Square (sqLoc, es)
          ]
    ]

parseCurly :: Parser [Zexpr]
parseCurly = do
  openCurly
  void $ MP.optional inlineSpace
  es <- MP.choice
    [ do
        e <- parseChain
        es <- MP.many (inlineSpace >> parseChain)
        pure (e:es)
    , pure []
    ]
  void $ MP.optional inlineSpace
  closeCurly
  pure es

{-
AtomExpr
  ::= Number
   |  <string>
   |  <symbol> | '\\' <!ws> <string>
-}
parseAtom :: Parser Zexpr
parseAtom = MP.choice
  [ parseSym
  , parseNum
  , withLocation dqStr <&> \(loc, s) -> ZAtom loc (Str s)
  ]

{-
Number ::= <int> | <float>
-}
parseNum :: Parser Zexpr
parseNum = withLocation (MP.choice
  [ Right <$> hexInt
  , decNum
  ]) <&> \(loc, num) -> case num of
    Right n -> ZAtom loc (Int n)
    Left repr -> ZCombo loc FloatLit repr

parseSym :: Parser Zexpr
parseSym = do
  bigPos0 <- MP.getSourcePos
  (loc, s) <- withLocation anySym
  ss <- MP.many $ MP.try $ do
    _ <- MP.single ':'
    withLocation anySym
  bigLoc <- adaptPos bigPos0 <$> MP.getSourcePos
  pure $ if null ss
    then ZAtom loc (Sym s)
    else ZCombo bigLoc QualName ((loc, s) : ss)
  where
  anySym = plainSym MP.<|> dqSym

------------------------------------ Whitespace ------------------------------------

data IndentState = IS
  { indentType :: Maybe Char -- WARNING either space or tab
  , indentStack :: NonEmpty Int
  }
  deriving (Show)

emptyIndentState :: IndentState
emptyIndentState = IS Nothing (0 :| [])

curDepth :: IndentState -> Int
curDepth IS{indentStack = (depth :| _)} = depth

pushDepth :: Int -> IndentState -> IndentState
pushDepth depth' st = st{indentStack = NE.cons depth' (indentStack st)}

popDepth :: IndentState -> IndentState
popDepth st = case indentStack st of
  0 :| [] -> st
  _ :| [] -> errorWithoutStackTrace "internal error: bottom od indent stack non-zero"
  _ :| (d:ds) -> st{indentStack = d :| ds}

nextline :: Parser ()
nextline = MP.try $ do
  st <- get
  (depth', indTy') <- leadingWhitespace (indentType st)
  MP.notFollowedBy MP.eof
  if curDepth st == depth'
    then put st{indentType = indTy'}
    else fail ""

indent :: Parser ()
indent = MP.try $ do
  st <- get
  (depth', indTy') <- MP.lookAhead $ leadingWhitespace (indentType st)
  if curDepth st < depth'
    then put $ pushDepth depth' st{indentType = indTy'}
    else fail ""

dedent :: Parser ()
dedent = MP.try $ do
  st <- get
  (depth', indTy') <- MP.lookAhead $ leadingWhitespace (indentType st)
  if depth' < curDepth st
    then put $ popDepth st{indentType = indTy'}
    else fail ""


inlineSpace :: Parser ()
inlineSpace = MP.choice
  [ simpleSpace >> void (MP.optional hangingSpace)
  , hangingSpace
  ]

hangingSpace :: Parser ()
hangingSpace = void $ MP.try $ do
  st <- get
  (depth', indTy') <- leadingWhitespace (indentType st)
  if curDepth st < depth'
    then put st{indentType = indTy'}
    else fail ""

leadingWhitespace :: Maybe Char -> Parser (Int, Maybe Char)
leadingWhitespace indTy = do
  -- skip trailing space and blank lines
  let loop = MP.choice
        [ MP.try $ do
            void $ MP.optional simpleSpace
            _ <- newline
            loop
        , MP.try $ do
            void $ MP.optional simpleSpace
            MP.eof
        , pure ()
        ]
  loop
  spaces <- case indTy of
    Nothing -> MP.takeWhileP (Just "spaces") (== ' ') MP.<|> MP.takeWhileP (Just "tabs") (== '\t')
    Just ' ' -> MP.takeWhileP (Just "spaces") (== ' ')
    Just '\t' -> MP.takeWhileP (Just "tabs") (== '\t')
    Just _ -> errorWithoutStackTrace "internal error: unexpected character stored in indent type"
  it <- case T.uncons spaces of
    Nothing -> pure (0, indTy)
    Just (c, _) -> pure (T.length spaces, Just c)
  MP.notFollowedBy (MP.oneOf (" \t" :: String))
  pure it

-- i.e. spaces, tabs, and comments
simpleSpace :: Parser ()
simpleSpace = MP.choice
  [ comment
  , void $ standard >> MP.optional comment
  ]
  where
  standard = MP.some $ MP.choice
    [ MP.takeWhile1P (Just "whitespace") (`elem` (" \t" :: String))
    , MP.chunk "\\ " -- for automatic tabulation (to "\\\t", because tabulation must be done with spaces
    , MP.try $ MP.single '\\' >> newline -- line continuation
    ]
  comment = do
    void $ MP.single '#'
    void $ MP.takeWhileP Nothing (`notElem` ("\n\r" :: String))
    pure ()

newline :: Parser Text
newline = MP.choice
  [ MP.chunk "\r\n"
  , T.singleton <$> MP.single '\n'
  , T.singleton <$> MP.single '\r'
  ]

------------------------------------ Tokens ------------------------------------

openRound, closeRound :: Parser ()
openRound = void $ MP.single '('
closeRound = void $ MP.single ')'

openSquare, closeSquare :: Parser ()
openSquare = void $ MP.single '['
closeSquare = void $ MP.single ']'

openCurly, closeCurly :: Parser ()
openCurly = void $ MP.single '{'
closeCurly = void $ MP.single '}'

comma :: Parser ()
comma = do
  _ <- MP.try (MP.optional inlineSpace >> MP.single ',')
  inlineSpace

consDot :: Parser ()
consDot = do
  _ <- MP.try (inlineSpace >> MP.single '.')
  inlineSpace

plainSym :: Parser Symbol
plainSym = do
  MP.notFollowedBy takeSign
  intern . T.unpack <$> MP.takeWhile1P (Just "symbol char") isSymChar

dqSym :: Parser Symbol
dqSym = MP.try $ do
  str <- MP.try $ MP.single '\\' >> dqStr
  pure $ intern (T.unpack str)


isSymChar :: Char -> Bool
isSymChar c
  =  isAlphaNum c
  || c `elem` ("~!%^&*-_=+<>/?" :: String)
  || c == '…'
  || c `elem` ("ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓ₀₁₂₃₄₅₆₇₈₉₊₋" :: String)
  || c `elem` ("ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻" :: String)
  || c `elem` ("ΓΔΘΛΞΠΣΤΥΦΨΩαβγδεζηθικλμνξοπρσςτυφχψω":: String)
  || c `elem` ("𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" :: String)

decNum :: Parser (Either (Integer, Int, Integer) Integer)
decNum = do
  sign <- takeSign
  wholeDigits <- takeDecDigits
  let whole = read wholeDigits
  decimalPoint <- MP.option False $ MP.try $ do
    _ <- MP.single '.'
    _ <- MP.lookAhead $ MP.satisfy isDigit
    pure True
  num <- if not decimalPoint
  then pure $ Right (sign * whole)
  else do
    fracDigits <- takeDecDigits
    let allDigits = wholeDigits ++ fracDigits
        sigDigits = dropTrailingZeros allDigits
        significand = read sigDigits
    let exponent = if significand == 0
                   then 0
                   else fromIntegral $ length allDigits - length sigDigits - length fracDigits
    pure $ Left (sign * significand, 10, exponent)
  MP.notFollowedBy (MP.satisfy isSymChar)
  pure num
  where
  dropTrailingZeros = (\s -> if null s then "0" else s) . reverse . dropWhile (== '0') . reverse

hexInt :: Parser Integer
hexInt = do
  sign <- MP.try $ do
    sign <- takeSign
    _ <- MP.chunk "0x" MP.<|> MP.chunk "0X"
    pure sign
  predigits <- do
    let takeHex = MP.takeWhile1P (Just "hex digit") isHexDigit
    d0 <- takeHex
    ds <- MP.many (MP.some (MP.single '_') >> takeHex)
    pure $ mconcat (d0:ds)
  let digits = (filter (/= '_') . T.unpack) predigits
      ((n,""):_) = readHex digits
  MP.notFollowedBy (MP.optional (MP.single ':') >> MP.satisfy isSymChar)
  pure (sign * n)

takeSign :: Parser Integer
takeSign = MP.try $ do
  sign <- MP.optional (MP.single '+' MP.<|> MP.single '-') <&> \case
    Nothing -> 1
    Just '+' -> 1
    Just '-' -> (-1)
    Just _ -> errorWithoutStackTrace "internal error: parsing sign"
  _ <- MP.lookAhead $ MP.satisfy isDigit
  pure sign

-- take decimal digits, interspersed with underscores, and return just the string of all the digits
takeDecDigits :: Parser String
takeDecDigits = do
  let takeDec = MP.takeWhile1P (Just "digit") isDigit
  d0 <- takeDec
  ds <- MP.many (MP.some (MP.single '_') >> takeDec)
  pure $ filter (/= '_') . T.unpack $ mconcat (d0:ds)

dqStr :: Parser Text
dqStr = do
  _ <- MP.single '\"'
  sections <- MP.many $ MP.choice
    [ MP.takeWhile1P Nothing (`notElem` ("\\\"\n" :: String))
    , do
      _ <- MP.single '\\'
      MP.choice
        [ MP.single 'a' >> pure "\a"
        , MP.single 'b' >> pure "\b"
        , MP.single 'e' >> pure "\ESC"
        , MP.single 'f' >> pure "\f"
        , MP.single 'n' >> pure "\n"
        , MP.single 'r' >> pure "\r"
        , MP.single 't' >> pure "\t"
        , MP.single 'v' >> pure "\v"
        , MP.single '\'' >> pure "\'"
        , MP.single '\"' >> pure "\""
        , MP.single '\\' >> pure "\\"
        , MP.single '^' >> MP.choice
          [ MP.single '?' >> pure "\x7F"
          , MP.satisfy (\c -> '@' <= c && c <= '_') <&> \c -> T.singleton (chr $ ord c - ord '@')
          ]
        , do
            h1 <- MP.satisfy isHexDigit
            h2 <- MP.satisfy isHexDigit
            let [(n,"")] = readHex [h1,h2]
            pure $ T.singleton (chr n)
        , do
            _ <- MP.oneOf ("uU" :: String)
            _ <- MP.optional $ MP.single '+'
            digits <- MP.takeWhile1P (Just "hex digit") isHexDigit
            let [(n,"")] = readHex (T.unpack digits)
            when (n > 0x10FFFF) $ fail "codepoint out of range"
            _ <- MP.single ';'
            pure $ T.singleton (chr n)
        , do
            _ <- MP.chunk "&#"
            digits <- MP.takeWhile1P (Just "digit") isDigit
            let n = read (T.unpack digits) :: Int
            when (n > 0x10FFFF) $ fail "codepoint out of range"
            _ <- MP.single ';'
            pure $ T.singleton (chr n)
        , do
            _ <- MP.try $ MP.optional inlineSpace >> newline
            _ <- MP.optional inlineSpace >> MP.single '\\'
            pure ""
        ]
    , do
        nl <- MP.try $ MP.optional inlineSpace >> newline
        _ <- MP.optional inlineSpace >> MP.single '\\'
        pure $ nl
    ]
  _ <- MP.single '\"'
  pure $ T.concat sections
