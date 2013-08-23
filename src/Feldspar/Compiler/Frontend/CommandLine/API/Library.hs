--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright notice, 
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Feldspar.Compiler.Frontend.CommandLine.API.Library where


import Data.Char (toUpper)
import Language.Haskell.Interpreter
import System.Console.ANSI
import Feldspar.Compiler.Backend.C.Library

upperFirst :: String -> String
upperFirst (first:rest) = toUpper first : rest
upperFirst s            = s

formatStringListCore :: [String] -> String
formatStringListCore []     = ""
formatStringListCore [x]    = x
formatStringListCore (x:xs) = x ++ " | " ++ formatStringListCore xs

formatStringList :: [String] -> String
formatStringList []   = error "formatStringList should not be called with an empty list."
formatStringList list = "(" ++ formatStringListCore list ++ ")"

rpad :: Int -> String -> String
rpad target = rpadWith target ' '

rpadWith :: Int -> Char -> String -> String
rpadWith target padchar s
    | length s >= target = s
    | otherwise = rpadWith target padchar (s ++ [padchar])

fancyWrite :: String -> IO ()
fancyWrite s = do
    withColor Blue $ putStr "=== [ "
    withColor Cyan $ putStr $ rpad 70 s
    withColor Blue $ putStrLn " ] ==="

withColor :: Color -> IO () -> IO ()
withColor color action = do
    setSGR [SetColor Foreground Vivid color, SetColor Background Dull Black] -- , SetConsoleIntensity BoldIntensity]
    action
    setSGR [Reset]
