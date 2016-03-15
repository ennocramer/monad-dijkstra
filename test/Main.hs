module Main ( main ) where

import           Control.Monad        ( mplus, mzero )
import           Control.Monad.Search

import           Test.Tasty
import           Test.Tasty.Hspec

data Side = L | R
    deriving (Eq, Show)

newtype C = C Int
    deriving (Eq, Ord, Show)

instance Monoid C where
    mempty = C 0
    mappend (C l) (C r) = C (l + r)

testSearch :: Search C Side -> [(C, Side)]
testSearch = runSearch

testSearchIO :: SearchT C IO Side -> IO [(C, Side)]
testSearchIO = runSearchT

spec :: IO TestTree
spec = testSpec "Control.Monad.Search" $ do
    it "Monad return generates one result" $
        testSearch (return L) `shouldBe` [ (C 0, L) ]

    it "MonadPlus mzero has no result" $
        testSearch mzero `shouldBe` []

    it "MonadPlus left identity law" $
        testSearch (mzero `mplus` return L) `shouldBe` [ (C 0, L) ]

    it "MonadPlus right identity law" $
        testSearch (return L `mplus` mzero) `shouldBe` [ (C 0, L) ]

    it "MonadPlus left distribution law" $
        testSearch (return L `mplus` return R) `shouldBe` [ (C 0, L), (C 0, R) ]

    it "Results are ordered by cost" $ do
        testSearch (return L `mplus` (cost' (C 1) >> return R))
            `shouldBe` [ (C 0, L), (C 1, R) ]
        testSearch ((cost' (C 1) >> return L) `mplus` return R)
            `shouldBe` [ (C 0, R), (C 1, L) ]

    it "Collapse suppresses results with higher cost" $
        testSearch ((collapse >> return L) `mplus` (cost' (C 1) >> return R))
            `shouldBe` [ (C 0, L) ]

    it "Collapse can be limited in scope" $
        testSearch (seal ((collapse >> return L) `mplus` (cost' (C 1) >> return R))
                    `mplus` (cost' (C 2) >> return R))
            `shouldBe` [ (C 0, L), (C 2, R) ]

    it "Results are generated lazily" $ do
        head (testSearch (return L `mplus`
                              (cost' (C 1) >> return (error "not lazy right"))))
            `shouldBe` (C 0, L)
        head (testSearch ((cost' (C 1) >> return (error "not lazy left")) `mplus`
                              return L))
            `shouldBe` (C 0, L)

    it "Results are generated lazily in IO" $ do
        head <$>
            testSearchIO (return L `mplus`
                              (cost' (C 1) >> return (error "not lazy right")))
                `shouldReturn` (C 0, L)
        head <$>
            testSearchIO ((cost' (C 1) >> return (error "not lazy left")) `mplus`
                              return L)
                `shouldReturn` (C 0, L)

main :: IO ()
main = do
    spec' <- spec
    defaultMain spec'
