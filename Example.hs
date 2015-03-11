import           Data.Functor
import           Data.List
import           System.Process
import           Test.CrossCheck
import           Test.QuickCheck

data JsNumber = JsInt Int
              | JsNaN
                deriving Show

instance Arbitrary JsNumber where
  arbitrary = frequency [ (1,  return JsNaN)
                        , (19, JsInt <$> choose (-2 ^ (53 :: Int), 2 ^ (53 :: Int)))
                        ]

instance CrossPrintable JsNumber where
  crossPrint JsNaN = ["NaN"]
  crossPrint (JsInt n) = [show n]

data JsList a = JsList [a] deriving Show

instance Arbitrary a => Arbitrary (JsList a) where
  arbitrary = JsList <$> arbitrary

instance CrossPrintable a => CrossPrintable (JsList a) where
  crossPrint (JsList xs)= ["[" ++ intercalate ", " (xs >>= crossPrint) ++ "]"]

nodejsCheck :: CrossCheck
nodejsCheck = CrossCheck (\c -> readProcess "node" ["-e", c] "")

reverse1 :: JsNumber -> CrossProperty
reverse1 =
  comparePrograms "console.log(require('./reverse')([ {0} ]));"
                  "console.log([ {0} ]);"

reverseFlip :: JsList JsNumber -> JsList JsNumber -> CrossProperty
reverseFlip xs ys =
  comparePrograms "console.log(require('./reverse')( {0}.concat( {1} ) ));"
                  "console.log(require('./reverse')( {1} ).concat(require('./reverse')( {0} )));"
                  (Cross2 xs ys)

main :: IO ()
main = do
  crossCheck nodejsCheck reverse1
  crossCheck nodejsCheck reverseFlip
