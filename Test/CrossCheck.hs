module Test.CrossCheck where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.List
import           Data.List.Split
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

newtype CrossProperty = CrossProperty (Crossed ())

data Crossed a = Crossed { runCrossed :: ReaderT CrossCheck (PropertyM IO) a }

instance Functor Crossed where
  fmap f (Crossed a) = Crossed (fmap f a)

instance Applicative Crossed where
  Crossed f <*> Crossed a = Crossed (f <*> a)
  pure a = Crossed (pure a)

instance Monad Crossed where
  Crossed a >>= f = Crossed (a >>= runCrossed . f)
  return a = Crossed (return a)

data CrossCheck = CrossCheck { crossCheckCommand :: String -> IO String }

class CrossPrintable a where
  crossPrint :: a -> [String]

data Cross2 a b = Cross2 a b
data Cross3 a b c = Cross3 a b c

instance (CrossPrintable a, CrossPrintable b) => CrossPrintable (Cross2 a b) where
  crossPrint (Cross2 a b) = crossPrint a ++ crossPrint b

instance (CrossPrintable a, CrossPrintable b, CrossPrintable c) => CrossPrintable (Cross3 a b c) where
  crossPrint (Cross3 a b c) = crossPrint a ++ crossPrint b ++ crossPrint c

class CrossTestable prop where
  crossProperty :: CrossCheck -> prop -> CrossProperty

instance CrossTestable CrossProperty where
  crossProperty _ = id

instance (Arbitrary a, Show a, CrossTestable prop) => CrossTestable (a -> prop) where
  crossProperty cc f = CrossProperty $ Crossed (lift (pick arbitrary)) >>= crossAssert . toProperty cc . crossProperty cc . f

crossFormat :: String -> [String] -> String
crossFormat s = foldl (\s' (n, a) -> intercalate a $ splitOn ("{" ++ show n ++ "}") s') s . zip [(0::Int)..]

runCross :: CrossPrintable a => String -> a -> Crossed String
runCross s args = Crossed (asks crossCheckCommand) >>= (\f -> Crossed . lift . run $ f command)
  where command = crossFormat s $ crossPrint args

crossAssert :: Property -> Crossed ()
crossAssert = Crossed . lift . stop

toProperty :: CrossCheck -> CrossProperty -> Property
toProperty cc (CrossProperty (Crossed p)) = monadicIO $ runReaderT p cc

comparePrograms :: CrossPrintable a => String -> String -> a -> CrossProperty
comparePrograms a b args = CrossProperty $ do
  a' <- runCross a args
  b' <- runCross b args
  crossAssert $ a' === b'

crossCheck :: CrossTestable prop => CrossCheck -> prop -> IO ()
crossCheck cc = quickCheck . toProperty cc . crossProperty cc
