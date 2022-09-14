module Main where

import Prelude
import qualified Clash.Prelude as CP
import Clash.Prelude(Signal, HiddenClockResetEnable)
import Test.Tasty
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.Hedgehog as TH
import Clash.Sigout

myMuxO :: Signal d Bool -> Signal d a -> Signal d a -> Sigout s d a -> SigoutM s ()
myMuxO b x y o = defSigout o (CP.mux b x y)

myMux :: Signal d Bool -> Signal d a -> Signal d a -> Signal d a
myMux b x y = runSigoutM $ do
    o <- newSigout
    myMuxO b x y o
    getSigout o

myFix :: (Signal d a -> Signal d a) -> Signal d a
myFix f = runSigoutM $ do
    o <- newSigout
    s <- getSigout o
    defSigout o (f s)
    return s

tog :: HiddenClockResetEnable d => Signal d Bool -> Signal d Bool
tog a = s where
    s = CP.register False $ (/=) <$> a <*> s

myTog :: HiddenClockResetEnable d => Signal d Bool -> Signal d Bool
myTog a = myFix $ CP.register False . (\s -> (/=) <$> a <*> s)

main :: IO ()
main = defaultMain $ testGroup "." [
        TH.testProperty "mux" $ H.property $ do
            b <- CP.fromList . return <$> H.forAll Gen.bool
            x <- CP.fromList . return <$> H.forAll Gen.bool
            y <- CP.fromList . return <$> H.forAll Gen.bool
            CP.sampleN @CP.System 1 (myMux b x y) H.=== CP.sampleN @CP.System 1 (CP.mux b x y),
        TH.testProperty "tog" $ H.property $ do
            l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
            let sl = CP.fromList l
            CP.sampleN @CP.System (length l) (myTog sl) H.=== CP.sampleN @CP.System (length l) (tog sl)
    ]

