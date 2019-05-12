{-# LANGUAGE LambdaCase #-}
module Options
  ( parseOpts
  , version
  , Options(..)
  )
where

import           Options.Applicative
import           Options.Applicative.Help.Pretty

data Offset = Decimal | Octal | Hexadecimal deriving (Show)
data Options = Args { number :: Int
                    , offset :: Maybe Offset
                    , files :: [FilePath]
                    }
             | Version
             deriving (Show)

offsetP :: Parser Offset
offsetP = option
  offsetReader
  (long "offset" <> short 'o' <> metavar "<offset>" <> helpDoc helpText)
 where
  offsetReader = eitherReader $ \case
    "d" -> Right Decimal
    "o" -> Right Octal
    "x" -> Right Hexadecimal
    _ ->
      Left "Offset must be 'd' (decimal), 'h' (hexadecimal), or 'o' (octal)."
  helpText =
    Just
      $  paragraph
           "Write each string preceded by its byte offset from the start of the file. The format shall be dependent on the single character used as the format option-argument:"
      <> hardline
      <> indent
           4
           (vsep
             [ text "d\t The offset shall be written in decimal."
             , text "o\t The offset shall be written in octal."
             , text "x\t The offset shall be written in hexadecimal."
             ]
           )
  paragraph = fillSep . map text . words

optionsP :: Parser Options
optionsP =
  Args
    <$> option
          auto
          (  long "number"
          <> short 'n'
          <> value 4
          <> showDefault
          <> help
               "Specify the minimum string length, where the number argument is a positive decimal integer."
          <> metavar "<number>"
          )
    <*> optional offsetP
    <*> some (strArgument (metavar "FILES"))
    <|> Version
    <$  flag' () (long "version" <> short 'v' <> help "Print the version ")

version :: String
version = "0.0.1"

parseOpts :: IO Options
parseOpts = execParser (info (optionsP <**> helper) description)
 where
  description = fullDesc <> header ("hstrs " <> version) <> progDesc
    "A modern alternative for strings, in haskell"
