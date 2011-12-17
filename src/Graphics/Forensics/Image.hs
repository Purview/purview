module Graphics.Forensics.Image where

import Data.Array

-- TODO implement a proper Image = Array Int Color and wrapper funcs
newtype Image = Image (Array Int Int)