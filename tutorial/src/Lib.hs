{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lib
    ( someFunc
    ) where

import Data.Monoid

someFunc :: IO ()
someFunc = putStrLn "someFunc"
type Window = Int

data Tiler
  = Horiz [Tiler]
  | Floating [Tiler] -- First is background
  | Reflect Tiler
  | FocusFull Tiler
  | Wrap Window
  | InputController Int (Maybe Tiler)
  | Monitor Int (Maybe Tiler)
  deriving (Eq, Show) -- (Functor, Foldable, etc)

countWindows :: Tiler -> Int
countWindows (Wrap window) = 1
countWindows (Horiz tilers) =
    foldl (\oldSum t -> oldSum + countWindows t) 0 tilers

countWindows (Floating tilers) =
    foldl (\oldSum t -> oldSum + countWindows t) 0 tilers

countWindows (Reflect tiler) = countWindows tiler
countWindows (FocusFull tiler) = countWindows tiler
countWindows (InputController _ mTiler) = 
    maybe 0 countWindows mTiler
countWindows (Monitor _ mTiler) = 
    maybe 0 countWindows mTiler

showWindows :: Tiler -> String
showWindows (Wrap window) = show window
showWindows (Horiz tilers) =
    foldl (\oldSum t -> oldSum ++ showWindows t) "" tilers

showWindows (Floating tilers) =
    foldl (\oldSum t -> oldSum ++ showWindows t) "" tilers

showWindows (Reflect tiler) = showWindows tiler
showWindows (FocusFull tiler) = showWindows tiler
showWindows (InputController _ mTiler) = 
    maybe "" showWindows mTiler
showWindows (Monitor _ mTiler) = 
    maybe "" showWindows mTiler

doToWindows :: a -> (a -> a -> a) -> (Window -> a) -> Tiler -> a
doToWindows none combine f = doToWindows'
    where
        doToWindows' (Wrap window) = f window
        doToWindows' (Horiz tilers) =
            foldl (\oldSum t -> oldSum `combine` doToWindows' t) none tilers

        doToWindows' (Floating tilers) =
            foldl (\oldSum t -> oldSum `combine` doToWindows' t) none tilers

        doToWindows' (Reflect tiler) = doToWindows' tiler
        doToWindows' (FocusFull tiler) = doToWindows' tiler
        doToWindows' (InputController _ mTiler) = 
            maybe none doToWindows' mTiler
        doToWindows' (Monitor _ mTiler) = 
            maybe none doToWindows' mTiler

doToWindowsMonoid :: Monoid a => (Window -> a) -> Tiler -> a
doToWindowsMonoid f = doToWindows'
    where
        doToWindows' (Wrap window) = f window
        doToWindows' (Horiz tilers) =
            foldl (\oldSum t -> oldSum <> doToWindows' t) mempty tilers

        doToWindows' (Floating tilers) =
            foldl (\oldSum t -> oldSum <> doToWindows' t) mempty tilers

        doToWindows' (Reflect tiler) = doToWindows' tiler
        doToWindows' (FocusFull tiler) = doToWindows' tiler
        doToWindows' (InputController _ mTiler) = 
            maybe mempty doToWindows' mTiler
        doToWindows' (Monitor _ mTiler) = 
            maybe mempty doToWindows' mTiler

showWindowsNew :: Tiler -> String
showWindowsNew = doToWindowsMonoid show

countWindowsNew :: Tiler -> Int
countWindowsNew = getSum . doToWindowsMonoid (const 1)


data TilerF t
  = HorizF [t]
  | FloatingF [t] -- First is background
  | ReflectF t
  | FocusFullF t
  | WrapF Window
  | InputControllerF Int (Maybe t)
  | MonitorF Int (Maybe t)
  deriving (Eq, Show, Functor, Foldable)

myTree :: TilerF (TilerF (TilerF t))
myTree = HorizF [FloatingF [WrapF 0], InputControllerF 5 (Just (WrapF 1)), FocusFullF (WrapF 2)]

showWindowsNewer :: TilerF (TilerF (TilerF t)) -> String
showWindowsNewer = undefined

newtype Fix f = Fix (f (Fix f))
type NewTiler = Fix TilerF
unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

bottomUp :: Functor f => (f a -> a) -> Fix f -> a
bottomUp function (Fix initial) = function (fmap (bottomUp function) initial)

countWindowsNewest :: NewTiler -> Int
countWindowsNewest = bottomUp count
  where count (WrapF _) = 1
        count tiler = sum tiler

showWindowsNewest :: NewTiler -> String
showWindowsNewest = bottomUp showIt
  where showIt (WrapF w) = show w
        showIt tiler = concat tiler
