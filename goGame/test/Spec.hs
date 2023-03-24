module Main (main) where
import Lib
--import Test.HUnit 
main :: IO ()
main = runTests

    --putStrLn "Test suite not yet implemented"

-- runTests1 :: IO ()
-- runTests1 =  do
--   _ <- runTestTT $ TestList [ testInitialBoardLength
--                               --testInitialGameState
--    ]
--   return ()


--test that the size is boardsize and all are nothing
-- testInitialBoardLength :: Test
-- testInitialBoardLength = "testInitialBoardLength" ~: TestList [
--   let boardSize = 19
--       board = initialBoard boardSize
--   in TestList [ TestCase $ assertEqual "board size" boardSize (length board)
--               , TestCase $ assertBool "board elements" (all (\row -> length row == boardSize && all (== Nothing) row) board)
--               ]
--   ]

-- testInitialGameState :: Test
-- testInitialGameState = "testInitialGameState" ~: do
--   let expectedGameState = GameState (initialBoard 19) (Just Black) (Just (0, 0)) (Just (0, 0)) 0
--   let actualGameState = initialGameState 19
--   assertEqual "Initial game state is incorrect" expectedGameState actualGameState