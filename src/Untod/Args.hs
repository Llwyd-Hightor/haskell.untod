module Untod.Args (
    utOpts
    ) where
import Untod.Version
import Untod.Data
import Options.Applicative
import Data.Semigroup ((<>))

utargs :: Parser Uargs
utargs = Uargs
      <$> (
            flag' TOD
          ( long "tod"
            <> short 'o'
            <> help "Convert from TOD (default)" )
        <|> flag' DATE
          ( long "date"
            <> short 'd'
            <> help "Convert from Date/Time" )
        <|> flag' PMC
          ( long "pmc"
            <> short 'm'
            <> help "Convert from PMC" )
        <|> flag' UNIX
          ( long "unix"
            <> short 'u'
            <> help "Convert from Unix seconds" )
        <|> flag' CSEC
          ( long "seconds"
            <> short 's'
            <> help "Convert from 20th Century seconds" )
        <|> pure TOD
          )
      <*> switch
          ( long "clipboard"
            <> short 'c'
            <> help "Input values from clipboard" )
      <*> switch
          ( long "csv"
            <> help "Output in CSV format" )
      <*> switch
          ( long "headers"
            <> help "Output column headers" )
      <*> switch
          ( long "annot"
            <> short 'a'
            <> help "Annotate plain output with run mode" )
      <*> switch
          ( long "zulu"
            <> short 'z'
            <> help "Suppress Zulu offset result if others given" )
      <*> (
            flag' LOR
          ( long "loran"
            <> short 'l'
            <> help "Ignore leap-seconds -- LORAN/IBM" )
        <|> flag' TAI
          ( long "tai"
            <> short 't'
            <> help "Ignore leap-seconds -- TAI (International Atomic Tick" )
        <|> pure UTC
          )
      <*> (
            flag' L
          ( long "lpad"
            <> help "Pad TOD with zeroes on left" )
        <|> flag' R
          ( long "rpad"
            <> help "Pad TOD with zeroes on right (default is intelligent padding)" )
        <|> pure I
          )
      <*> ( optional $ strOption
            (  long "input"
            <> short 'i'
            <> metavar "<filename>"
            <> help "Input values from a file ( - for STDIN )" )
            )
      <*> ( optional $ option auto
            (  long "lzone"
            <> metavar "<offset>"
            <> help "Override local time offset ([-+]n.n) [env: UNTOD_LZONE=]" )
            )
      <*> ( optional $ option auto
            (  long "azone"
            <> metavar "<offset>"
            <> help "Alternative time offset ([-+]n.n) [env: UNTOD_AZONE=]" )
            )
      <*> ( many $ argument str
            (  metavar "<value...>"
            <> help "Values for conversion" )
            )
      <*>   ( length <$> many
              (flag' ()
                ( long "version" <> short 'v'
                <> help "Show version; more -v flags, more info")
              )
            )

utOpts = info (utargs <**> helper)
    ( fullDesc
    <> progDesc ("Converts among TOD, Date/Time, PARS Perpetual Minute Tick, "
    ++ "Unix seconds, and 20th century seconds for UTC, TAI or LORAN/IBM")
    <> header ("untod " ++ utVersion ++ " - a Swiss Army knife for TOD and other clocks")
    )
