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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Backend.C.Options where

import Data.Typeable
import Text.Show.Functions

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend

data Options =
    Options
    { platform          :: Platform
    , unroll            :: UnrollStrategy
    , debug             :: DebugOption
    , memoryInfoVisible :: Bool
    , rules             :: [Rule]
    } deriving (Eq, Show)

data UnrollStrategy = NoUnroll | Unroll Int
    deriving (Eq, Show)

data DebugOption = NoDebug | NoPrimitiveInstructionHandling
    deriving (Eq, Show)

data Platform = Platform {
    name            :: String,
    types           :: [(Type, String, String)],
    values          :: [(Type, ShowValue)],
    includes        :: [String],
    platformRules   :: [Rule],
    isRestrict      :: IsRestrict
} deriving (Eq, Show)

type ShowValue = Constant () -> String

instance Eq ShowValue where
    (==) _ _ = True

data IsRestrict = Restrict | NoRestrict
    deriving (Show,Eq)

-- * Actions and rules

data Action t
    = Replace t
    | Propagate Rule
    | WithId (Int -> [Action t])
    | WithOptions (Options -> [Action t])
  deriving Typeable

data Rule where
    Rule :: (Typeable t) => (t -> [Action t]) -> Rule

instance Show Rule where
    show _ = "Transformation rule."

instance Eq Rule where
    _ == _ = False

rule :: (Interface t, Typeable (Repr t)) => (t -> [Action (Repr t)]) -> Rule
rule f = Rule $ \x -> f $ toInterface x

replaceWith :: (Interface t) => t -> Action (Repr t)
replaceWith = Replace . fromInterface

propagate :: (Interface t, Typeable (Repr t)) => (t -> [Action (Repr t)]) -> Action t'
propagate = Propagate . rule

-- Belongs in some other module, but temporarily resides here to avoid
-- cyclic imports.
type Position = (Int, Int)

-- Belongs in some other module, but temporarily resides here to avoid
-- cyclic imports.
data CompToCCoreResult t = CompToCCoreResult {
    sourceCode      :: String,
    endPosition     :: Position,
    debugModule     :: Module t
}
