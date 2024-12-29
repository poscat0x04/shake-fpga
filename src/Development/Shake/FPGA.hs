module Development.Shake.FPGA
  ( -- * Program as a library

    -- | Since clash requires the clash-prelude package used to compile clash
    --   and used in clash designs to be the exact same package, you can include
    --   this library in your cabal file to ensure they are the same.
    shakeMain,

    -- * Re-exports
    module Development.Shake.FPGA.Internal,
  )
where

import Development.Shake.FPGA.Internal
  ( BuildConfig (..),
    CompiledBuildConfig (..),
    HDL (..),
    Target (..),
    TargetRef,
    rulesFromFile,
    shakeFromFile,
  )
import Options.Applicative
import System.Environment (withArgs)

shakeMain :: IO ()
shakeMain = do
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
