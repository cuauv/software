-- Haskell SHM Interface

{-# LANGUAGE OverloadedStrings #-}

module SHM.Interface where

import qualified Data.Text.IO as T
import qualified CPython as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py

init = do
    Py.initialize
    Py.importModule "shm"

getAttribute shm attr = \f -> Py.callArgs f [] =<< Py.getAttribute shm =<< Py.toUnicode attr
