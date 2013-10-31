{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Semantics.SimPOL.Management where
import Language.POL.Syntax
import Semantics.POL.Management as POL
import Data.POL.Observable ( ObservableT, runObservableT )
import Data.SimPOL.Time as Time ( Zero ( .. ), Discrete ( .. ) )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Printf
import Text.Dialog
type Management t = ManagementT t IO
evolve :: (Monad m, Zero t) => ManagementT t m a -> m a
evolve = evolveWithTime Time.zero
evolveWithTime :: Monad m => t -> ManagementT t m a -> m a
evolveWithTime = flip (evalStateT . runManagementT)
now :: Monad m => ManagementT t m t
now = ManagementT get
advance :: (Monad m, Discrete t) => ManagementT t m ()
advance = ManagementT (modify Time.advance)
newtype ManagementT t m a = ManagementT { runManagementT :: StateT t m a }
instance Functor m => Functor (ManagementT t m) where
  fmap f = ManagementT . fmap f . runManagementT
instance Monad m => Monad (ManagementT t m) where
  return  = ManagementT . return
  m >>= n = ManagementT $ (runManagementT . n =<< runManagementT m)
  fail    = ManagementT . fail
instance MonadIO m => MonadIO (ManagementT t m) where
  liftIO  = lift . liftIO
instance MonadTrans (ManagementT t) where
  lift = ManagementT . lift
instance (Ord a, Ord p, Ord l, Show a, Show p, Show l, MonadIO m, Show t) => POL.Management a p l (ManagementT t m) where
  use a p = do
    t <- now
    liftIO $ putStrLn $ printf ("(%s) Using data %s "
      ++ "for purpose %s.") (show t) (show a) (show p)
    return Zero
  send Zero           = return Zero
  send (Data a p)     = do
    t <- now
    liftIO $ putStrLn $ printf ("(%s) Sending data %s "
      ++ "for purpose %s "
      ++ "to the contract party.") (show t) (show a) (show p)
    return Zero
  send (Give _)       = return Zero
  send (And c1 c2)    = send c1 >> send c2
  send (c1 `Or` c2)   = send c1 >> send c2
  send (If      _ c)  = send c
  send (IfNot   _ c)  = send c
  send (When    _ c)  = send c
  send (Anytime _ c)  = send c
  send (Until   _ c)  = send c
  greedy c1 c2 = do
    t   <- now
    sw  <- liftIO $ dialog (printf ("(%s) "
      ++ "Your contract options are:\n"
      ++ "  1. %s\n  2. %s\nWhich contract do you choose")
      (show t) (show c1) (show c2)) ["1","2"]
    case sw of
      "1" -> execute c1
      "2" -> execute c2
      _   -> greedy c1 c2
  ifthenelse o c1 c2 = do
    sw <- runObservableT o
    if sw
      then execute c1
      else execute c2
  when o c = do
    sw <- runObservableT o
    if sw
      then execute c
      else return $ When o c
  stopping o c = do
    t   <- now
    sw  <- runObservableT o
    if sw
      then do
        sw' <- liftIO $ dialog
          ( printf ("(%s) Do you want to execute "
            ++ "the contract (%s) now") (show t) (show c)
          ) ["yes", "no"]
        case sw' of
          "yes" -> execute c
          _     -> return $ Anytime o c
      else return $ Anytime o c
  absorb o c = do
    t   <- now
    sw  <- runObservableT o
    if sw
      then return Zero
      else do
        sw' <- liftIO $ dialog
          ( printf ("(%s) Do you want to execute "
            ++ "the contract (%s) now") (show t) (show c)
          ) ["yes","no"]
        case sw' of
          "yes" -> execute c
          _     -> return $ Until o c
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
