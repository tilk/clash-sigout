{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Clash.Sigout(
    Sigout, SigoutT, SigoutM,
    runSigoutT, runSigoutM,
    newSigout, defSigout, getSigout
) where

import Clash.Prelude
import qualified Data.HKey as HK
import qualified Data.HMap as HM
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Trans as T

type Sigout s d a = HK.HKey s (Signal d a)

newtype SigoutT s m a = SigoutT { fromSigoutT :: StateT HM.HMap (ReaderT HM.HMap (HK.KeyT s m)) a }
    deriving newtype (Functor, Applicative, Monad)

type SigoutM s a = SigoutT s Identity a

runSigoutT :: MonadFix m => (forall s. SigoutT s m a) -> m a
runSigoutT m = fmap fst $ mfix $ \ ~(_, s) -> HK.runKeyT $ runReaderT (runStateT (fromSigoutT m) HM.empty) s

runSigoutM :: (forall s. SigoutM s a) -> a
runSigoutM m = runIdentity $ runSigoutT m

newSigout :: Monad m => SigoutT s m (Sigout s d a)
newSigout = SigoutT $ T.lift $ T.lift $ HK.newKey

defSigout :: Monad m => Sigout s d a -> Signal d a -> SigoutT s m ()
defSigout o v = SigoutT $ modify (HM.insert o v)

getSigout :: Monad m => Sigout s d a -> SigoutT s m (Signal d a)
getSigout o = SigoutT $ do
    s <- T.lift $ ask
    return $ HM.findWithDefault (error "Sigout not defined") o s

