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

{-# LANGUAGE OverlappingInstances #-}

module Feldspar.Compiler.Imperative.Plugin.CollectFreeVars where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Feldspar.Compiler.Imperative.Frontend as Front
import Feldspar.Transformation

data Collect = Collect

type Collection = Map String Front.Var

instance Default Collection where
    def = Map.empty

instance Combine Collection where
    combine = Map.union

instance Transformation Collect
  where
    type From Collect     = ()
    type To Collect       = ()
    type Down Collect     = ()
    type Up Collect       = Collection
    type State Collect    = ()

instance Plugin Collect
  where
    type ExternalInfo Collect = ()
    executePlugin _ externalInfo = result . transform Collect () externalInfo

instance Transformable Collect Variable where
    transform _ _ _ v = Result v () $ Map.singleton (varName v) (Front.toInterface v)

instance Transformable Collect Block where
    transform t _ _ bl = Result bl' () diff
      where
        Result bl' _ m = defaultTransform t () () bl
        localNames = map (varName . declVar) $ locals bl'
        localMap = Map.fromList $ map (\name -> (name,())) localNames
        diff = m `Map.difference` localMap

instance Transformable Collect Program where
    transform t _ _ pr@(ParLoop idxVar _ _ _ _ _) = Result pr' () diff
      where
        Result pr' _ m = defaultTransform t () () pr
        diff = Map.delete (varName idxVar) m
    transform t _ _ prog = defaultTransform t () () prog
