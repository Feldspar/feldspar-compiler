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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Backend.C.Plugin.Rule
    ( RulePlugin(..)
    ) where

import Data.Typeable

import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.Options

data RulePlugin = RulePlugin

instance Transformation RulePlugin
  where
    type From RulePlugin     = ()
    type To RulePlugin       = ()
    type Down RulePlugin     = Options
    type Up RulePlugin       = [Rule]
    type State RulePlugin    = Int

instance Plugin RulePlugin
  where
    type ExternalInfo RulePlugin = Options
    executePlugin _ externalInfo = result . transform RulePlugin 0 externalInfo

instance (DefaultTransformable RulePlugin t, Typeable1 t) => Transformable RulePlugin t where
    transform t s d orig = recurse { result = x'', up = pr1 ++ pr2 ++ pr3, state = newID2 }
      where
        recurse = defaultTransform t s d orig
        applyRule :: t () -> Int -> [Rule] -> (t (), [Rule], [Rule], Int)
        applyRule c x = foldl applyRuleFun (c,[],[],x)
          where
            applyRuleFun :: (t (), [Rule], [Rule], Int) -> Rule -> (t (), [Rule], [Rule], Int)
            applyRuleFun (cc,incomp,prop,currentID) (Rule r) = case cast r of
                Nothing -> (cc,incomp ++ [Rule r],prop, currentID)
                Just r' -> (cc',incomp,prop ++ prop', newID)
                  where
                    (cc',prop', newID) = applyAction cc currentID (r' cc)
                    applyAction :: t () -> Int -> [Action (t ())] -> (t (), [Rule], Int)
                    applyAction ccc cid = foldl applyActionFun (ccc,[],cid)
                      where
                        applyActionFun :: (t (), [Rule], Int) -> Action (t ()) -> (t (), [Rule], Int)
                        applyActionFun (_     , prop'', i) (Replace newConstr) = (newConstr, prop''        , i)
                        applyActionFun (ccc'  , prop'', i) (Propagate pr)      = (ccc'     , prop'' ++ [pr], i)
                        applyActionFun (constr, _     , i) (WithId f)          = applyAction constr (i + 1) (f i)
                        applyActionFun (constr, _     , i) (WithOptions f)     = applyAction constr i       (f d)
        (x',_,pr1,newID1) = applyRule (result recurse) (state recurse) (rules d)
        (x'',pr3,pr2,newID2) = applyRule x' newID1 (up recurse)


instance Combine [Rule] where
    combine = (++)
