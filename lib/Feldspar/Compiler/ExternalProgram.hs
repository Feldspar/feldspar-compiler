module Feldspar.Compiler.ExternalProgram
  ( icompileFile
  , compileFile
  )
  where

import qualified Data.ByteString.Char8 as B

import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Backend.C.Options (Options(..))
import Feldspar.Compiler.Imperative.Representation (Module(..))
import Feldspar.Compiler.Imperative.ExternalProgram (parseFile, massageInput)
import Feldspar.Compiler.Frontend.Interactive.Interface (writeFiles)

icompileFile :: FilePath -> IO ()
icompileFile filename = do
  let hfilename = filename ++ ".h"
      cfilename = filename ++ ".c"
  h <- B.readFile hfilename
  c <- B.readFile cfilename
  let comp = compileFile' defaultOptions (hfilename, h) (cfilename, c)
  case comp of
    (Nothing, _) -> putStrLn $ "Could not parse " ++ hfilename
    (_, Nothing) -> putStrLn $ "Could not parse " ++ cfilename
    (Just hprg, Just cprg) -> putStrLn $ sourceCode cprg


compileFile :: FilePath -> FilePath -> String -> Options -> IO ()
compileFile fileName outFile funName opts = do
  let hfilename = fileName ++ ".h"
      cfilename = fileName ++ ".c"
  h <- B.readFile hfilename
  c <- B.readFile cfilename
  let comp = compileFile' defaultOptions (hfilename, h) (cfilename, c)
  case comp of
    (Nothing, _) -> print $ "Could not parse " ++ hfilename
    (_, Nothing) -> putStrLn $ "Could not parse " ++ cfilename
    (Just hprg, Just cprg) -> writeFiles prg outFile funName opts
      where prg = SplitModule cprg hprg

compileFile' :: Options -> (String, B.ByteString) -> (String, B.ByteString)
            -> (Maybe CompiledModule, Maybe CompiledModule)
compileFile' opts (hfilename, hfile) (cfilename, cfile) =
  case parseFile hfilename hfile [] of
    Nothing -> (Nothing, Nothing)
    Just hprg -> case parseFile cfilename cfile (entities hprg) of
                   Nothing -> (Just hres, Nothing)
                   Just cprg -> (Just hres', Just cres)
                     where res = compileToCCore' opts cprg
                           cres = implementation res
                           -- Un-duplicated hres.
                           hres' = interface res
      where -- Will result in duplicate function declarations, but we
            -- just failed parsing the c file and return nothing for
            -- that so the user is probably not that picky on
            -- potential duplicate declarations if they had succeeded.
            hres = interface $ compileToCCore' opts hprg

