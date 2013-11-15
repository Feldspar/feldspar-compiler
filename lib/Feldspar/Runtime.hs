-- | A module provinding a linker hook that can be referenced to ensure
-- that the runtime support files are linked.
--
-- NOTE. This file should not be loaded from ghci
--

module Feldspar.Runtime
    ( feldspar_compiler_hook
    )
  where

feldspar_compiler_hook :: Int
feldspar_compiler_hook = sum [ feldspar_c99_hook
                             , feldspar_ivar_hook
                             , feldspar_taskpool_hook
                             ]

foreign import ccall safe "feldspar_c99_hook"
  feldspar_c99_hook :: Int

foreign import ccall safe "feldspar_ivar_hook"
  feldspar_ivar_hook :: Int

foreign import ccall safe "feldspar_taskpool_hook"
  feldspar_taskpool_hook :: Int

