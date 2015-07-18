module Data.DOM.File(files,filesArray,name) where

import Data.Foreign.EasyFFI
import Data.Foreign

import Data.Maybe

import DOM
import DOM.File

ffi = unsafeForeignFunction

files :: Node -> Maybe FileList
files = ffi ["node"] "node.files" -- noooo

filesArray :: FileList -> Array File
filesArray = ffi ["fileList"] "fileList"

name :: File -> String
name = ffi ["file"] "file.name"

--instance fileList FileList
