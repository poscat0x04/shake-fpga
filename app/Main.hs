{-# LANGUAGE ApplicativeDo #-}

module Main where

import Development.Shake.FPGA (shakeFromFile)
import Options.Applicative
import System.Environment (withArgs)

main :: IO ()
main = do
  (config, rest) <- execParser opts
  withArgs rest $ shakeFromFile config
  where
    parser = do
      config <-
        strOption
          ( metavar "CONFIG"
              <> long "config"
              <> short 'c'
              <> help "The configuration file to use (default: shake-fpga.yaml)"
              <> value "shake-fpga.yaml"
          )
      rest <-
        many
          ( strArgument
              ( metavar "ARGS"
                  <> help "arguments to pass to shake (use -- to pass arguments with leading -)"
              )
          )
      pure (config, rest)
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Build clash based FPGA projects using shake"
        )
