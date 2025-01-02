module Development.Shake.FPGA.DirStructure
  ( DirStructure (..),
    BuildOutputLayout (..),
    dirsOf,
    buildOutputsOf,
    depMakefile,
  )
where

import Development.Shake.FilePath ((</>))
import GHC.Generics (Generic)

data DirStructure = DirStructure
  { clashDir :: FilePath,
    vivadoDir :: FilePath,
    verilatorDir :: FilePath,
    verilatedObjDir :: FilePath,
    hsDir :: FilePath
  }
  deriving (Show, Eq, Generic)

data BuildOutputLayout = BuildOutputLayout
  { manifestFile :: FilePath,
    tclScript :: FilePath,
    bitStreamFile :: FilePath,
    libverilatedA :: FilePath,
    libVmodelA :: FilePath,
    vmodelH :: FilePath,
    vmodelCCPP :: FilePath,
    vmodelCH :: FilePath,
    vmodelCO :: FilePath,
    -- static archive of Vmodel-c
    libVmodelCA :: FilePath,
    -- hsc file, binds to Vmodel-c
    verilatedHSC :: FilePath,
    -- relocatable object file that merges all objects in libVmodelCA
    fullVmodelCO :: FilePath
  }
  deriving (Show, Eq, Generic)

depMakefile :: String -> FilePath
depMakefile modName = "_build" </> modName </> "deps.mk"

dirsOf :: (String, String) -> DirStructure
dirsOf (moduleId, topEntityId) = DirStructure {..}
  where
    buildDir = "_build"
    targetId = moduleId </> topEntityId
    clashDir = buildDir </> targetId </> "clash"
    vivadoDir = buildDir </> targetId </> "vivado"
    verilatorDir = buildDir </> targetId </> "verilator"
    verilatedObjDir = buildDir </> targetId </> "verilated"
    hsDir = buildDir </> targetId </> "haskellBinding"

buildOutputsOf :: (String, String) -> BuildOutputLayout
buildOutputsOf t = BuildOutputLayout {..}
  where
    DirStructure {..} = dirsOf t
    manifestFile = clashDir </> "clash-manifest.json"
    tclScript = vivadoDir </> "synth+implement.tcl"
    bitStreamFile = vivadoDir </> "out.bit"

    vmodelH = verilatorDir </> "Vmodel.h"
    libverilatedA = verilatorDir </> "libverilated.a"
    libVmodelA = verilatorDir </> "libVmodel.a"

    vmodelCCPP = verilatorDir </> "Vmodel-c.cpp"
    vmodelCH = verilatorDir </> "Vmodel-c.h"
    vmodelCO = verilatorDir </> "Vmodel-c.o"
    libVmodelCA = verilatedObjDir </> "libVmodel-c.a"
    fullVmodelCO = verilatedObjDir </> "Vmodel-c.o"

    verilatedHSC = hsDir </> "Verilated.hsc"
