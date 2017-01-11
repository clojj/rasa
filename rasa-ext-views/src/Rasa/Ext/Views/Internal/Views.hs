{-# language FlexibleInstances, TemplateHaskell, DeriveFunctor #-}
module Rasa.Ext.Views.Internal.Views
  (
  getViews
  , refocusView
  , rotate
  , viewPayload
  , splitRule
  , active
  , closeBy
  , focusViewLeft
  , focusViewRight
  , focusViewAbove
  , focusViewBelow
  , windows
  , hSplit
  , vSplit
  , Dir(..)
  , SplitRule(..)
  , Window
  , Split(..)
  , Views(..)
  , View(..)
  , BiTree(..)
  , BiTreeF(..)
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.BiTree

import Control.Lens
import Data.Default
import Data.Functor.Foldable

data SplitRule =
  Ratio Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

instance Default SplitRule where
  def = Ratio 0.5

data Dir = Hor
         | Vert
         deriving (Show)

instance Default Dir where
  def = Vert

data Split = Split
  { _dir :: Dir
  , _splitRule :: SplitRule
  } deriving (Show)
makeLenses ''Split

instance Default Split where
  def = Split def def

data View a = View
  { _active :: Bool
  , _viewPayload :: a
  } deriving (Show, Functor)

makeLenses ''View

instance Default (View Int) where
  def = View True 0

split :: Dir -> SplitRule -> Window -> Window -> Window
split d sr = Branch (Split d sr)

viewport :: Bool -> Int -> Window
viewport act bi = Leaf $ View act bi

type Window = BiTree Split (View Int)

data Views = Views
  { _windows' :: Window
  }
makeLenses ''Views

windows :: HasEditor e => Lens' e Window
windows = ext.windows'

instance Show Views where
  show _ = "Views"

instance Default Views where
  def = Views $ split Vert (Ratio 0.5)
                              (viewport True 0)
                              $ split Hor (Ratio 0.5)
                                  (viewport False 1)
                                  (viewport False 1)

rotate :: Window -> Window
rotate = cata alg
  where alg (LeafF vw) = Leaf vw
        alg (BranchF sp s e) = Branch (sp & dir %~ rotDir) s e
        rotDir Hor = Vert
        rotDir Vert = Hor

splitView :: Dir -> Window -> Window
splitView d = cata alg
  where alg (LeafF vw) = if vw ^. active
                            then Branch (Split d def) (Leaf vw) (Leaf (vw & active .~ False))
                            else Leaf vw
        alg b = embed b

hSplit, vSplit :: Window -> Window
hSplit = splitView Hor
vSplit = splitView Vert

closeBy :: (View Int -> Bool) -> Window -> Window
closeBy p = zygo par alg
  where
    par (LeafF vw) = not $ p vw
    par (BranchF _ l r) = l || r
    alg (LeafF vw) = Leaf vw
    alg (BranchF sp (keepLeft, l) (keepRight, r))
      | keepLeft && keepRight = Branch sp l r
      | keepLeft = l
      | keepRight = r
      | otherwise = Leaf def

focusViewLeft :: Window -> Window
focusViewLeft = zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Hor _) l r) = l || r
    par (BranchF (Split Vert _) l _) = l
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Hor _) (_, l) (_, r)) = Branch sp l r
    alg (BranchF sp@(Split Vert _) (_, l) (fromRight, r)) =
      Branch sp left r
        where left = if fromRight
                        then l & taking 1 (backwards traverse) . active .~ True
                        else l

focusViewRight :: Window -> Window
focusViewRight = zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Hor _) l r) = l || r
    par (BranchF (Split Vert _) _ r) = r
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Hor _) (_, l) (_, r)) = Branch sp l r
    alg (BranchF sp@(Split Vert _) (fromLeft, l) (_, r)) =
      Branch sp l right
        where right = if fromLeft
                         then r & taking 1 traverse . active .~ True
                         else r

focusViewAbove :: Window -> Window
focusViewAbove = zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Vert _) u d) = u || d
    par (BranchF (Split Hor _) u _) = u
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Vert _) (_, u) (_, d)) = Branch sp u d
    alg (BranchF sp@(Split Hor _) (_, u) (fromBottom, d)) =
      Branch sp top d
        where top = if fromBottom
                        then u & taking 1 (backwards traverse) . active .~ True
                        else u

focusViewBelow :: Window -> Window
focusViewBelow = zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Vert _) u d) = u || d
    par (BranchF (Split Hor _) _ d) = d
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Vert _) (_, u) (_, d)) = Branch sp u d
    alg (BranchF sp@(Split Hor _) (fromTop, u) (_, d)) =
      Branch sp u bottom
        where bottom = if fromTop
                         then d & taking 1 traverse . active .~ True
                         else d

refocusView :: Window -> Window
refocusView = taking 1 traverse . active .~ True

getViews :: Action Views
getViews = use ext
