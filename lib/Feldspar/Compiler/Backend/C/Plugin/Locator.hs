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

{-# LANGUAGE TypeFamilies #-}

module Feldspar.Compiler.Backend.C.Plugin.Locator where

import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.Plugin.PrettyPrint


-- ===========================================================================
--  == GetPrg plugin
-- ===========================================================================


instance Default Bool where
    def = False

instance Default (Program DebugToCSemanticInfo) where
    def = Empty ((0,0),(0,0)) ((0,0),(0,0))

instance Combine Bool where
    combine l1 l2 = l1 || l2

instance Combine (Program DebugToCSemanticInfo) where
    combine Empty{} p2 = p2
    combine p1      _  = p1

-----------------------------------------------------
--- GetPrg plugin for ParLoop
-----------------------------------------------------

data GetPrgParLoop = GetPrgParLoop

instance Transformation GetPrgParLoop where
    type From GetPrgParLoop    = DebugToCSemanticInfo
    type To GetPrgParLoop      = DebugToCSemanticInfo
    type Down GetPrgParLoop    = (Int, Int)  
    type Up GetPrgParLoop      = (Bool, Program DebugToCSemanticInfo)
    type State GetPrgParLoop   = ()


instance Plugin GetPrgParLoop where
    type ExternalInfo GetPrgParLoop = (Int, Int)
    executePlugin GetPrgParLoop (line, col) procedure =
        result $ transform GetPrgParLoop () (line, col) procedure

getPrgParLoop :: (Int, Int) -> Module DebugToCSemanticInfo -> (Bool, Program DebugToCSemanticInfo)
getPrgParLoop (line, col) procedure = up res where
    res = transform GetPrgParLoop () (line, col) procedure        

instance Transformable GetPrgParLoop Program where
    transform t () (line, col) pl@(ParLoop _ _ _ prog inf1 _) = Result pl () info where
        info  = case contains (line, col) inf1  of
                    True -> infoCr where
                        res = transform t () (line, col) prog
                        infoCr = if fst $ up res then up res else (True, pl)
                    _    -> def
    transform t () (line, col) pr = defaultTransform t () (line, col) pr

-----------------------------------------------------
--- GetPrg plugin for Assign
-----------------------------------------------------

data GetPrgAssign = GetPrgAssign

instance Transformation GetPrgAssign where
    type From GetPrgAssign    = DebugToCSemanticInfo
    type To GetPrgAssign      = DebugToCSemanticInfo
    type Down GetPrgAssign    = (Int, Int)  
    type Up GetPrgAssign      = (Bool, Program DebugToCSemanticInfo)
    type State GetPrgAssign   = ()


instance Plugin GetPrgAssign where
    type ExternalInfo GetPrgAssign = (Int, Int)
    executePlugin GetPrgAssign (line, col) procedure =
        result $ transform GetPrgAssign () (line, col) procedure

getPrgAssign :: (Int, Int) -> Module DebugToCSemanticInfo -> (Bool, Program DebugToCSemanticInfo)
getPrgAssign (line, col) procedure = up res where
    res = transform GetPrgAssign () (line, col) procedure        

instance Transformable GetPrgAssign Program where
    transform _ () (line, col) assign@(Assign _ _ inf1 _) = Result assign () info where
        info  = if contains (line, col) inf1 then (True, assign) else def
    transform t () (line, col) pr = defaultTransform t () (line, col) pr


-----------------------------------------------------
--- GetPrg plugin for Branch
-----------------------------------------------------

data GetPrgBranch = GetPrgBranch

instance Transformation GetPrgBranch where
    type From GetPrgBranch    = DebugToCSemanticInfo
    type To GetPrgBranch     = DebugToCSemanticInfo
    type Down GetPrgBranch   = (Int, Int)  
    type Up GetPrgBranch      = (Bool, Program DebugToCSemanticInfo)
    type State GetPrgBranch   = ()


instance Plugin GetPrgBranch where
    type ExternalInfo GetPrgBranch = (Int, Int)
    executePlugin GetPrgBranch (line, col) procedure =
        result $ transform GetPrgBranch () (line, col) procedure

getPrgBranch :: (Int, Int) -> Module DebugToCSemanticInfo -> (Bool, Program DebugToCSemanticInfo)
getPrgBranch (line, col) procedure = up res where
    res = transform GetPrgBranch () (line, col) procedure        

instance Transformable GetPrgBranch Program where
    transform t () (line, col) br@(Branch _ prog1 prog2 inf1 _) = Result br () info where
        info  = case contains (line, col) inf1  of
                    True -> infoCr where
                        res1 = transform t () (line, col) prog1
                        res2 = transform t () (line, col) prog2
                        res = combine (up res1) (up res2)
                        infoCr = if fst res then res else (True,br)
                    _    -> def

    transform t () (line, col) pr = defaultTransform t () (line, col) pr

-----------------------------------------------------
--- GetPrg plugin for ProcedureCall
-----------------------------------------------------

data GetPrgProcCall = GetPrgProcCall

instance Transformation GetPrgProcCall where
    type From GetPrgProcCall    = DebugToCSemanticInfo
    type To GetPrgProcCall      = DebugToCSemanticInfo
    type Down GetPrgProcCall    = (Int, Int)  
    type Up GetPrgProcCall      = (Bool, Program DebugToCSemanticInfo)
    type State GetPrgProcCall   = ()


instance Plugin GetPrgProcCall where
    type ExternalInfo GetPrgProcCall = (Int, Int)
    executePlugin GetPrgProcCall (line, col) procedure =
        result $ transform GetPrgProcCall () (line, col) procedure

getPrgProcCall :: (Int, Int) -> Module DebugToCSemanticInfo -> (Bool, Program DebugToCSemanticInfo)
getPrgProcCall (line, col) procedure = up res where
    res = transform GetPrgProcCall () (line, col) procedure        

instance Transformable GetPrgProcCall Program where
    transform _ () (line, col) pc@(ProcedureCall _ _ inf1 _) = Result pc () info where
        info  = if contains (line,col) inf1 then (True,pc) else def
    transform t () (line, col) pr = defaultTransform t () (line, col) pr

-----------------------------------------------------
--- GetPrg plugin for SeqLoop
-----------------------------------------------------

data GetPrgSeqLoop = GetPrgSeqLoop

instance Transformation GetPrgSeqLoop where
    type From GetPrgSeqLoop    = DebugToCSemanticInfo
    type To GetPrgSeqLoop      = DebugToCSemanticInfo
    type Down GetPrgSeqLoop    = (Int, Int)  
    type Up GetPrgSeqLoop      = (Bool, Program DebugToCSemanticInfo)
    type State GetPrgSeqLoop   = ()


instance Plugin GetPrgSeqLoop where
    type ExternalInfo GetPrgSeqLoop = (Int, Int)
    executePlugin GetPrgSeqLoop (line, col) procedure =
        result $ transform GetPrgSeqLoop () (line, col) procedure

getPrgSeqLoop :: (Int, Int) -> Module DebugToCSemanticInfo -> (Bool, Program DebugToCSemanticInfo)
getPrgSeqLoop (line, col) procedure = up res where
    res = transform GetPrgSeqLoop () (line, col) procedure

instance Transformable GetPrgSeqLoop Program where
    transform t () (line, col) sl@(SeqLoop _ _ prog inf1 _) = Result sl () info where
        info  = case contains (line, col) inf1  of
                    True -> infoCr where
                        res = transform t () (line, col) prog
                        infoCr = if fst $ up res then up res else (True, sl)
                    _    -> def
    transform t () (line, col) pr = defaultTransform t () (line, col) pr


-------------------------------------------------
------ Helper functions
-------------------------------------------------

contains :: (Ord a, Ord b) => (a,b) -> ((a,b),(a,b)) -> Bool
contains (line, col) ((bl, bc), (el, ec)) = (line == bl && bc <= col) || (bl < line && line < el) || (line == el && col <= ec)

myShow :: Program DebugToCSemanticInfo -> String
myShow (Assign l r inf1 _) = "Assign \n" ++ ind show l ++ "\n=\n" ++ ind show r ++ "\n" ++ show inf1 ++ "\n"
myShow (Sequence progs inf1 _) = "Sequence\n" ++ ind (listprint myShow "\n") progs ++ "\n" ++ show inf1 ++ "\n"
myShow (Branch _ tb eb inf1 _) = "Branch\n" ++ ind myShowB tb ++ "\nelse\n" ++ ind myShowB eb ++ "\n" ++ show inf1 ++ "\n"
myShow (ParLoop count bound step block inf1 _)
    = "ParLoop\n count: " ++ show count ++ "\n bound: " ++ show bound ++ "\n step: " ++ show step ++ "\n" ++ ind myShowB block ++ "\n" ++ show inf1 ++ "\n"
myShow x = show x

myShowB :: Block DebugToCSemanticInfo -> String
myShowB (Block ls prg inf) = "Block\n" ++ ind show ls ++"\n" ++ ind myShow prg ++ show inf

