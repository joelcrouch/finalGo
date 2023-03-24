module Lib
    ( someFunc, boardSize,initialBoard,initialGameState,showBoard,
      showMaybeStone,
      parseMove,
      isOnBoard,
      switchPlayer,
      getScore,
      oppositeStone,
      territories,
      neighborPositions,
      otherPlayer,
      groupPositionsByColor,
      boardPositions,
      liberties,
      neighborPositionsByColor,
      validPos,
      floodfill,
      checkGameOver,
      hasAir,
      getAdjacentPositions,
      getStone,
      stones,
      replace,
      neighborPositions',
      isCaptured,
      isCapturedGroup,
      oppositeStoneG,
      --combineCaptureStones,
      captureStone,
      groupPositions, 
      isCapturedGroup,
      setNothing,
      gameLoop,
      runTests,
      Stone,
      Board,
      GameState,
      Position,
      PositionState,
      Group
    ) where

import Test.HUnit 
import Data.Function
import System.IO
import Data.Char
import Data.Maybe
import Data.List 
import Text.Read
import Debug.Trace (trace)
import Data.Either
import Data.IORef
-- import Data.List.Split

data Stone = Black | White | Nil deriving (Eq, Show)
type Board = [[Maybe Stone]]
data GameState = GameState
  { board :: Board
  , currentPlayer :: Maybe Stone
  , capturedStones :: Maybe (Int, Int)
  , score :: Maybe (Int, Int)
  , passCount :: Int
  } deriving (Eq, Show)
  
type Position = (Int, Int)
type Group = [(Position, Maybe Stone)]
data Direction = North | South | East | West

data PositionState = Empty | Filled Stone deriving (Eq, Show)

boardSize :: Int 
boardSize = 19 

initialBoard :: Int -> Board
initialBoard boardSize = replicate boardSize (replicate boardSize Nothing)

testInitialBoardLength = "testInitialBoardLength" ~: TestList [
  let boardSize = 19
      board = initialBoard boardSize
  in TestList [ TestCase $ assertEqual "board size" boardSize (length board)
              , TestCase $ assertBool "board elements" (all (\row -> length row == boardSize && all (== Nothing) row) board)
              ]
  ]

initialGameState :: Int -> GameState
initialGameState size = GameState (initialBoard size) (Just Black) (Just (0,0)) (Just (0,0)) 0

testInitialGameState :: Test
testInitialGameState = "testInitialGameState" ~: do
  let expectedGameState = GameState (initialBoard 19) (Just Black) (Just (0, 0)) (Just (0, 0)) 0
  let actualGameState = initialGameState 19
  assertEqual "Initial game state is incorrect" expectedGameState actualGameState



showBoard :: Board -> String
showBoard board =
  let
    size = boardSize
    rows = map (\r -> map showMaybeStone r) board
    numberedRows = zip [1..size] rows
    numberedCols = take (length board)['a'..'t']
    colNumbers = "  " ++ take size (unwords (map (\c -> " " ++ [c] ++ " ") (take size numberedCols)))
    --colNumbers = "  " ++ take size (unwords (map (\c -> " " ++ [c] ++ " ") numberedCols))
    boardRows = map (\(rowNum, row) -> (show rowNum) ++ " " ++ unwords row) numberedRows
  in
    unlines (colNumbers : boardRows)


testShowBoard :: Test
testShowBoard =  "testShowBoard " ~: TestCase $ do
  let board = [[Just Black, Just White, Nothing], [Nothing, Just Black, Just White], [Just Black, Nothing, Just White]]
      expectedOutput = unlines ["   a   b   c ", "1 B W .", "2 . B W", "3 B . W"]
      actualOutput = showBoard board
  assertEqual "The showBoard function should format the board correctly" expectedOutput actualOutput


showMaybeStone :: Maybe Stone -> String
showMaybeStone Nothing = "."
showMaybeStone (Just Black) = "B"
showMaybeStone (Just White) = "W"
--found this on a website about building grids in haskell
parseMove :: String -> Maybe Position
parseMove [col, row] = do
  colIndex <- elemIndex col ['a'..'t']
  rowIndex <- readMaybe [row] :: Maybe Int
  let position = (rowIndex - 1, colIndex)
  if isOnBoard position
    then Just position
    else Nothing
parseMove _ = Nothing

testParseMove :: Test
testParseMove = "parseMove" ~: TestList
    [ isJust (parseMove "a1") ~? "Valid move 'a1' should be parsed"
    , isNothing (parseMove "t19") ~? "InValid move 't19' should not be parsed"
    , isNothing (parseMove "x3") ~? "Invalid column 'x' should not be parsed"
    , isNothing (parseMove "a20") ~? "Invalid row '20' should not be parsed"
    , isNothing (parseMove "a0") ~? "Invalid row '0' should not be parsed"
    ]

isOnBoard :: Position -> Bool
isOnBoard  (x, y) = x >= 0 && y >= 0 && x < boardSize && y < boardSize

testIsOnBoard :: Test
testIsOnBoard = "testIsOnBoard" ~:
  TestList
    [ TestCase $ assertBool "Position (0, 0) should be on a 19x19 board." (isOnBoard (0, 0))
    , TestCase $ assertBool "Position (19, 19) should not be on a 19x19 board." (not $ isOnBoard (19, 19))
    , TestCase $ assertBool "Position (-1, 5) should not be on a 19x19 board." (not $ isOnBoard (-1, 5))
    ]


switchPlayer :: GameState -> GameState
switchPlayer state =
  let newPlayer = oppositeStone (currentPlayer state)
  in state { currentPlayer = newPlayer }

testSwitchPlayer :: Test
testSwitchPlayer =
  let initialState = GameState (initialBoard 19) (Just Black) Nothing Nothing 0
      expectedState = GameState (initialBoard 19) (Just White) Nothing Nothing 0
      newState = switchPlayer initialState
  in "switchPlayer switches the current player" ~: expectedState ~=? newState

oppositeStone :: Maybe Stone -> Maybe Stone
oppositeStone (Just Black) = Just White
oppositeStone (Just White) = Just Black
oppositeStone _ = Nothing

testOppositeStone :: Test
testOppositeStone = "testOppositeStone" ~: TestList
  [ oppositeStone (Just Black) ~?= Just White
  , oppositeStone (Just White) ~?= Just Black
  , oppositeStone Nothing ~?= Nothing
  ]

boardPositions :: Board -> [Position]
boardPositions board = [(x, y) | x <- [0..boardSize-1], y <- [0..boardSize-1], isOnBoard (x, y)]

testBoardPositions :: Test
testBoardPositions = "testBoardPositions" ~: testList
  where
    board = initialBoard boardSize
    expectedPositions = [(x, y) | x <- [0..boardSize-1], y <- [0..boardSize-1], isOnBoard (x, y)]
    actualPositions = boardPositions board
    testList = [
      assertEqual "Board positions are incorrect" expectedPositions actualPositions
      ]

  


-- see https://www.markhneedham.com/blog/2012/04/07/algorithms-flood-fill-in-haskell/
floodfill :: Board -> Position -> [Position] -> [Position]
floodfill board (x, y) visited
  | not (isOnBoard (x, y)) = visited
  | Just stone /= target = visited
  | (x, y) `elem` visited = visited
  | otherwise = foldr (floodfill board) ((x, y):visited) neighbors
  where
    stone = getStone board (x, y)
    target = Nothing
    neighbors = filter (\pos -> pos `notElem` visited) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]




checkGameOver :: GameState -> Maybe Stone
checkGameOver state =
  let
    board' = board state
    stones' = stones board'
    whiteStones = filter ((==) (Just White) . snd) stones'
    blackStones = filter ((==) (Just Black) . snd) stones'
    isBoardFull = all (isJust . snd) stones'
    hasWhiteAir = any (\(pos, _) -> hasAir board' pos White) whiteStones
    hasBlackAir = any (\(pos, _) -> hasAir board' pos Black) blackStones
  in
    case getScore state of
      (blackScore, whiteScore)
        | not (hasWhiteAir && hasBlackAir) || isBoardFull ->
          if whiteScore > blackScore then Just White
          else if blackScore > whiteScore then Just Black
          else Nothing
        | otherwise -> Nothing


testCheckGameOver = test [    
  "Game not over when both players have at least one liberty" ~: checkGameOver notGameOver ~?= Nothing,    
  "Game over when board is full" ~: checkGameOver gameOverBoardFull ~?= Just White,    
  "Empty board should not be game over" ~: checkGameOver emptyGameState ~?= Nothing,   
   "Black wins by score" ~: checkGameOver blackWinsByScore ~?= Just Black,    "White wins by score" ~: checkGameOver whiteWinsByScore ~?= Just White,    "Black wins by capture" ~: checkGameOver blackWinsByCapture ~?= Just Black,    "White wins by capture" ~: checkGameOver whiteWinsByCapture ~?= Just White  ]
  where
    emptyGameState = initialGameState 4
    blackWinsByScore = emptyGameState { board = [                              [   Just Black, Just White, Just White, Just Black],
       [Just Black, Just White, Just Black, Just Black],
       [Just Black, Just White, Just White, Just Black],
       [Just Black, Just White, Just Black, Just Black]
      ]
    , score = Just (14, 6)
    }
    whiteWinsByScore = emptyGameState { board = [                               [    Just White, Just Black, Just Black, Just White],
        [Just White, Just Black, Just White, Just White],
        [Just White, Just Black, Just Black, Just White],
        [Just White, Just Black, Just White, Just White]
      ]
      , score = Just (6, 14)
    }
    blackWinsByCapture = emptyGameState { board = [                             [   Just Black, Just White,  Nothing, Just Black],
       [Just Black, Just White, Just Black, Just Black],
       [Just Black, Just White, Nothing, Just Black],
       [Just Black, Just White, Just Black, Just Black]
      ]
      , capturedStones = Just (4, 0)
    }
    whiteWinsByCapture = emptyGameState { board = [                             [    Just White, Just Black,  Nothing, Just White],
        [Just White, Just Black, Just White, Just White],
        [Just White, Just Black,  Nothing, Just White],
        [Just White, Just Black, Just White, Just White]
      ]
      , capturedStones = Just (0, 4)
    }
    notGameOver = emptyGameState { board = [                                       [Just Black, Just White, Just White, Just Black],
                                       [Just Black, Just White, Just Black, Just Black],
                                       [Just Black, Just White, Nothing, Just Black],
                                       [Just Black, Just White, Just Black, Just Black]
                                     ]
                                  }
    gameOverBoardFull = emptyGameState { board = [                                            [Just Black, Just White, Just Black, Just White],
                                            [Just Black, Just White, Just Black, Just White],
                                            [Just White, Just Black, Just White, Just Black],
                                            [Just White, Just Black, Just White, Just Black]
                                           ]
                                         , score = Just (8, 8)
                                         }



-- get all the stones on the board  added a 'size' param for testing
stones :: Board -> [(Position, Maybe Stone)]
stones board = [((x, y), getStone board (x, y)) | x <- [0..size], y <- [0..size]]
  where
    size = length board - 1

testStones :: Test
testStones = "testStones" ~:
  TestList
    [ TestCase $ assertEqual "stones of an empty board should be Nothings"
        [((0,0),Nothing),((0,1),Nothing),((0,2),Nothing),((1,0),Nothing),((1,1),Nothing),((1,2),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Nothing)] (stones emptyBoard)
    , TestCase $ assertEqual "stones of a board with one black stone should have one black stone"
        [((0,0),Nothing),((0,1),Nothing),((0,2),Nothing),((1,0),Nothing),((1,1),Nothing),((1,2),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Just Black)] (stones $ setStone emptyBoard Black (2, 2) )
    , TestCase $ assertEqual "stones of a board with one white stone should have one white stone"
        [((0,0),Nothing),((0,1),Nothing),((0,2),Nothing),((1,0),Nothing),((1,1),Nothing),((1,2),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Just White)] (stones $ setStone emptyBoard White (2, 2) )
    , TestCase $ assertEqual "stones of a board with one black and one white stone should have two stones"
        [((0,0),Nothing),((0,1),Nothing),((0,2),Nothing),((1,0),Nothing),((1,1),Nothing),((1,2),Nothing),((2,0),Nothing),((2,1),Nothing),((2,2),Just Black)] (stones $ setStone (setStone emptyBoard White (3, 3) ) Black (2, 2) )
    ]
  where 
    emptyBoard = initialBoard 3

hasAir :: Board -> Position -> Stone -> Bool
hasAir board position stone =
  let
    adjacentPositions = getAdjacentPositions position
    adjacentStones = map (getStone board) adjacentPositions
    hasAdjacentAir = any isNothing adjacentStones
  in
    case getStone board position of
      Just s -> s == stone && hasAdjacentAir
      _ -> False

testHasAir :: Test
testHasAir =
  let
    board = setStone (setStone (initialBoard 19)  Black (0, 1))  White (1, 0)
  in
    test
      [ "hasAir with air" ~: hasAir board (0, 0) Black ~?= False
      , "hasAir without air" ~: hasAir board (0, 1) Black ~?= False
      , "hasAir with same color stones" ~: hasAir board (0, 1) White ~?= True
      ]

getAdjacentPositions :: Position -> [Position]
getAdjacentPositions (x, y) =
  [ (x + dx, y + dy) | (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]
                     , isOnBoard (x + dx, y + dy)
  ]

testGetAdjacentPositions :: Test
testGetAdjacentPositions =
  test
    [ "getAdjacentPositions 1" ~: getAdjacentPositions (0, 0) ~?= [(0, 1), (1, 0)]
    , "getAdjacentPositions 2" ~: getAdjacentPositions (1, 1) ~?= [(1, 2), (2, 1), (1, 0), (0, 1)]
    , "getAdjacentPositions 3" ~: getAdjacentPositions (18, 18) ~?= [(18, 17), (17, 18)]
    ]

getScore :: GameState -> (Int, Int)
getScore state =
  let
    board' = board state
    stones' = stones board'
    whiteStones = filter ((==) (Just White) . snd) stones'
    blackStones = filter ((==) (Just Black) . snd) stones'
    whiteTerritories = territories board' White
    blackTerritories = territories board' Black
    whiteScore = length whiteStones + length whiteTerritories
    blackScore = length blackStones + length blackTerritories
  in
    (blackScore, whiteScore)

testGetScore :: Test
testGetScore = "getScore" ~: test [
    "should return the correct score for a game state" ~:
      let board' = [[Nothing, Just White, Just Black], [Just Black, Just White, Just White], [Nothing, Just Black, Just White]]
          gameState = GameState { board = board', currentPlayer = Just White, capturedStones = Nothing, score = Nothing, passCount = 0 }
      in getScore gameState @?= (3, 4)
  ]




-- The function works by finding all the empty positions on the board that are adjacent to the specified player's stones and recursively exploring from those positions to find all the positions that belong to the player's territory.

-----------------------------
-- Given a board and a stone color, returns a list of positions on the board
-- where the given stone has surrounded an empty space and established a territory, i have to write alot of comments for this one. B/c i am in the weeds
territories :: Board -> Stone -> [(Int, Int)]
territories board stone = map fst $ filter isTerritory emptySpaces
  where
    -- A territory is defined as an empty space on the board where all of its
    -- adjacent positions are occupied by stones of the same color, and there
    -- are no enemy stones adjacent to it.
    isTerritory (pos, adjacentStones) =
      all ((/=) (Just stone) . snd) adjacentStones &&  -- no same color stone neighbors
      all ((/=) (Just (otherPlayer stone)) . snd) adjacentStones  -- no enemy stone neighbors
      where
        adjacentStones = filter (isAdjacent pos . fst) (stones board) -- get adjacent stones
    emptySpaces = filter ((==) Nothing . snd) (stones board) -- get empty spaces

testTerritories :: Test
testTerritories = "testTerritories" ~:
  TestList
    [ TestCase $ assertEqual "a single territory on the board"
        [(0,3),(1,3),(2,0),(2,1),(2,2),(2,3),(3,0),(3,1),(3,2),(3,3)] (territories singleTerritoryBoard Black)
    , TestCase $ assertEqual "multiple Stone on the board"
        [(0,3),(3,0),(3,1)] (territories multiTerritoryBoard Black)
    ]
  where
    emptyBoard = initialBoard 4
    singleTerritoryBoard = setStone emptyBoard  Black (1, 0) 
    multiTerritoryBoard = setStone (setStone singleTerritoryBoard White (0, 1) )  Black (3, 2) 

   -- multiTerritoryBoard = setStone singleTerritoryBoard (0, 1) White |> setStone emptyBoard (1, 2) Black
  
    
isAdjacent :: Position -> Position -> Bool
isAdjacent (x1, y1) (x2, y2) =
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
  in dx <= 1 && dy <= 1 && (dx + dy) /= 0 && isOnBoard (x2, y2)

testIsAdjacent :: Test
testIsAdjacent = "testIsAdjacent" ~:
  TestList
    [ TestCase $ assertBool "The positions (0, 0) and (0, 1) should be adjacent." (isAdjacent (0, 0) (0, 1))
    , TestCase $ assertBool "The positions (0, 0) and (1, 0) should be adjacent." (isAdjacent (0, 0) (1, 0))
    , TestCase $ assertBool "The positions (0, 0) and (1, 1) should be adjacent." (isAdjacent (0, 0) (1, 1))
    , TestCase $ assertBool "The positions (0, 0) and (2, 2) should not be adjacent." (not $ isAdjacent (0, 0) (2, 2))
    , TestCase $ assertBool "The positions (0, 0) and (0, 0) should not be adjacent." (not $ isAdjacent (0, 0) (0, 0))
    ]

groupPositionsByColor :: Board -> Stone -> [[(Int, Int)]]
groupPositionsByColor board player =
  let
    positions = boardPositions board
    groups = groupBy (\a b -> getStone board a == getStone board b) positions
    filteredGroups = filter (\group -> getStone board (head group) == Just player) groups
  in
    filteredGroups

neighborPositionsByColor :: Board -> Maybe Stone -> Position -> [Position]
neighborPositionsByColor board mColor (x, y) =
  let
    positions = [(x+dx, y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], abs dx /= abs dy]
    validPos' = validPos board mColor
  in
    filter validPos' positions

validPos :: Board -> Maybe Stone -> Position -> Bool
validPos board mColor (x,y) = isOnBoard (x,y) && (getStone board (x,y) == mColor)

neighborPositions :: Position -> Direction -> Position
neighborPositions (x,y) direction =
  case direction of
    North -> (x, y-1)
    South -> (x, y+1)
    East -> (x+1, y)
    West -> (x-1, y)

testNeighborPositions :: Test
testNeighborPositions =
  TestList
    [ "neighborPositions North" ~: neighborPositions (2,2) North ~?= (2,1)
    , "neighborPositions South" ~: neighborPositions (2,2) South ~?= (2,3)
    , "neighborPositions East"  ~: neighborPositions (2,2) East  ~?= (3,2)
    , "neighborPositions West"  ~: neighborPositions (2,2) West  ~?= (1,2)
    ]

otherPlayer :: Stone -> Stone
otherPlayer Black = White
otherPlayer White = Black

testOtherPlayer :: Test
testOtherPlayer = "testOtherPlayer" ~: do
  assertEqual "Expected otherPlayer Black to be White" White (otherPlayer Black)
  assertEqual "Expected otherPlayer White to be Black" Black (otherPlayer White)

getStone :: Board -> Position -> Maybe Stone
getStone board (x, y)
  | not (isOnBoard (x, y)) = Nothing
  | otherwise = board !! y !! x

testGetStone :: Test
testGetStone = "testGetStone" ~: TestList [
  "getStone returns Nothing for out-of-bounds position on one side" ~:
    Nothing ~?= getStone (initialBoard 19) (-1, 0),
  "getStone returns Nothing for out-of-bounds position on the other" ~:
    Nothing ~?= getStone (initialBoard 19) (0, -1),
  "getStone returns Nothing for out-of-bounds position on top with one out of bounds" ~:
    Nothing ~?= getStone (initialBoard 19) (19, 0),
  "getStone returns Nothing for out-of-bounds position the other out of bounds" ~:
    Nothing ~?= getStone (initialBoard 19) (0, 19),
  "getStone returns Just stone for a valid position" ~:
    let board = setStone (initialBoard 19)  Black (3, 3)
    in Just Black ~?= getStone board (3, 3),
  "getStone returns Just stone for a valid position" ~:
    let board = setStone (initialBoard 19)  White (5, 5)
    in Just White ~?= getStone board (5, 5)
  ]

gameLoop :: Int -> GameState -> IO ()
gameLoop size state = do
  putStrLn $ showBoard (board state)
  case checkGameOver state of
    Just winner -> putStrLn $ "Game over! " ++ show winner ++ " wins!"
    Nothing -> do
      putStr $ show (currentPlayer state) ++ ", nter your move or type 'pass': "
      input <- getLine
      case input of
        "pass" -> do
          let newState = GameState (board state) (oppositeStone (currentPlayer state)) (capturedStones state) (score state) (passCount state + 1)
          gameLoop size newState
        _ -> case parseMove input of
          Nothing -> do
            putStrLn "Illegal move! Try again."
            gameLoop size state
          Just position -> do
            let newState = playMove state position
            either
              (\_ -> do
                putStrLn "Illegal move! Try again."
                gameLoop size state)
              (\s -> gameLoop size (switchPlayer s))
              newState

testGameLoop :: Test
testGameLoop = TestLabel "Game loop" $ TestCase $ do
  let initialState = initialGameState 19
  gameLoop 19 initialState

-- testGameLoopTie = TestCase $ do
--   let initState = initialGameState 19
--       stateAfterFirstPass = GameState (board initState) (Just Black) (Just (0, 0)) (Just (0 0)) 1
--       stateAfterSecondPass = GameState (board initState) (Just White) (Just (0, 0)) (Just (0 0)) 2
--   gameLoop 19 stateAfterSecondPass
--   let result = checkGameOver stateAfterSecondPass --(winner initState)
--   assertEqual "Expected a tie" Nothing result

playMove :: GameState -> Position -> Either String GameState
playMove state position =
  let
    b = board state
    stone = fromMaybe Black (currentPlayer state)
    passNum = passCount state
    bc' = fromMaybe 0 (fst <$> capturedStones state)
    wc' = fromMaybe 0 (snd <$> capturedStones state)
    (blackCaptured, whiteCaptured, newBoard) = updateBoard b stone position
    newCaptured = case (blackCaptured, whiteCaptured) of
      (Just blackStones, Just whiteStones) ->
        case stone of
          Black -> Just (blackStones + bc', wc')
          White -> Just (bc', whiteStones + wc')
      _ -> capturedStones state
    newScore = case (blackCaptured, whiteCaptured) of
      (Just blackStones, Just whiteStones) ->
        let
          bc = fromMaybe 0 (fst <$> score state)
          wc = fromMaybe 0 (snd <$> score state)
          newBlackScore = bc + blackStones
          newWhiteScore = wc + whiteStones
        in Just (newBlackScore, newWhiteScore)
      _ -> score state
  
  in
    Right $ GameState newBoard (oppositeStone (currentPlayer state)) newCaptured newScore passNum

testPlayMove :: Test
testPlayMove = test [
    "playMove captures stones and updates score" ~: do
        let initialState = initialGameState boardSize
            -- position1 = Position 1 1
            -- position2 = Position 2 2
            newState1 = fromRight initialState $ playMove initialState (1,1)
            newState2 = fromRight initialState $ playMove newState1 (2,2)
            --print newState2
            expectedCaptured = Just (0, 0)
            expectedScore = Just (0, 0)
        assertEqual "captured stones" expectedCaptured (capturedStones newState2)
        assertEqual "updated score" expectedScore (score  newState2)
  ]

updateBoard :: Board -> Stone -> Position -> (Maybe Int, Maybe Int, Board)
updateBoard board stone pos =
  let
    oldStone = getStone board pos
    newBoard = setStone board stone pos
    neighbors = neighborPositions' pos
    (blackCaptured, whiteCaptured, finalBoard) = if any (isCaptured newBoard stone) neighbors
      then let (bc, wc, b') = captureStones newBoard stone neighbors in (Just bc, Just wc, b')
      else (Just 0, Just 0, newBoard)
  in
    case oldStone of
      Nothing -> (blackCaptured, whiteCaptured, finalBoard)
      Just s -> if s == stone then (blackCaptured, whiteCaptured, finalBoard) else error "Invalid move"

testUpdateBoard = test [
    "Place black stone" ~:
        let initialBoard = [[Nothing, Just White, Nothing],
                            [Nothing, Just Black, Nothing],
                            [Nothing, Just White, Nothing]]
        in updateBoard initialBoard Black (0, 1) ~?= (Just 0, Just 0, [[Nothing, Just Black, Nothing],                                                             [Nothing, Just Black, Nothing],
        [Nothing, Just White, Nothing]])
    ,
    "Capture no white stones" ~:
        let initialBoard = [[Just Black, Just White, Nothing],
                            [Just White, Nothing, Nothing],
                            [Nothing, Nothing, Nothing]]
        in updateBoard initialBoard Black (0, 0) ~?= (Just 0, Just 0, [[Just Black,Just White, Nothing],
        [Just White, Nothing, Nothing],
        [Nothing, Nothing, Nothing]])
    -- ,
    -- "Invalid move - occupied position" ~:
    --     let initialBoard = [[Just Black, Just White],
    --                         [Nothing, Nothing]]
    --     in updateBoard initialBoard Black (0, 0) `shouldThrow` anyException
    
    ]

setStone :: Board -> Stone -> Position -> Board
setStone board stone (x,y) = replace x newRow board
  where
    newRow = replace y (Just stone) (board !! x)

testSetStone :: Test
testSetStone = "testSetStone" ~: TestList [
    "setStone replaces empty cell with a stone" ~:
      expected ~=? setStone initial  Black (1, 1)
  ]
  where
    initial = initialBoard 4 --replicate 4 (replicate 4 Nothing)
    expected = [ [Nothing, Nothing, Nothing, Nothing]
               , [Nothing, Just Black, Nothing, Nothing]
               , [Nothing, Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing, Nothing]
               ]
 

replace :: Int -> a -> [a] -> [a]
replace n newVal (x:xs)
  | n == 0    = newVal:xs
  | otherwise = x:replace (n-1) newVal xs
replace _ _ [] = []

testReplace :: Test
testReplace = "testReplace" ~: TestList [
  "replace returns expected result for valid input" ~:
    [1, 5, 3, 4] ~?= replace 1 5 [1, 2, 3, 4],
  "replace returns original list if index is out of bounds" ~:
    [1, 2, 3, 4] ~?= replace 4 5 [1, 2, 3, 4],
  "replace returns original list if index is negative" ~:
    [1, 2, 3, 4] ~?= replace (-1) 5 [1, 2, 3, 4]
  ]

neighborPositions' :: Position -> [Position]
neighborPositions' (x, y) = filter isOnBoard [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

testNeighborPositions' :: Test
testNeighborPositions' =
  TestList [ "Top left corner" ~: neighborPositions' (0, 0) ~?= [(1, 0), (0, 1)]
           , "Top row" ~: neighborPositions' (0, 1) ~?= [(0, 0), (1, 1), (0, 2)]
           , "Top right corner" ~: neighborPositions' (0, 18) ~?= [(1, 18), (0, 17)]
           , "Right column" ~: neighborPositions' (1, 18) ~?= [(0, 18), (1, 17), (2, 18)]
           , "Bottom right corner" ~: neighborPositions' (18, 18) ~?= [(18, 17), (17, 18)]
           , "Bottom row" ~: neighborPositions' (18, 17) ~?= [(17, 17), (18, 16), (18, 18)]
           , "Bottom left corner" ~: neighborPositions' (18, 0) ~?= [(17, 0), (18, 1)]
           , "Left column" ~: neighborPositions' (1, 0) ~?= [(0, 0), (1, 1), (2, 0)]
           , "Middle position" ~: neighborPositions' (9, 9) ~?= [(8, 9), (10, 9), (9, 8), (9, 10)]
           ]

isCaptured :: Board -> Stone -> Position -> Bool
isCaptured board stone pos = null $ liberties board pos
  where
    liberties :: Board -> Position -> [Position]
    liberties b p = filter (isEmpty b) (neighborPositions' p)
    isEmpty :: Board -> Position -> Bool
    isEmpty b p = isNothing (getStone b p)

testIsCaptured :: Test
testIsCaptured = "isCaptured" ~: test [
    let board = initialBoard 3
        pos1 = (0,0)
        pos2 = (0,1)
        pos3 = (1,0)
        pos4 = (1,1)
        board' = setStone board Black pos1
        board''= setStone board White pos2
        tboard = setStone board'' White pos3
        final =  setStone tboard White pos4
    in do
      assertEqual "Test 1" False (isCaptured final Black pos2)
      assertEqual "Test 2" False (isCaptured final Black pos3)
      assertEqual "Test 3" False (isCaptured final Black pos4)
      assertEqual "Test 4" True  (isCaptured final White pos1)
      assertEqual "Test 5" False  (isCaptured final  Black (2,2))
  ]

captureStones :: Board -> Stone -> [Position] -> (Int, Int, Board)
captureStones board stone positions =
  let
    (captured, newBoard) = foldr (\pos (captured, b) -> captureStone stone captured b pos) ([], board) positions
    bc = length $ filter (==Black) captured
    wc = length $ filter (==White) captured
  in
    (bc, wc, newBoard)

testCaptureStones = test [
    "Capture multiple stones" ~:
        let initialBoard =  [[Just Black,Just White],
                            [Just White,Nothing]]
        in captureStones initialBoard Black [(0,1), (1,0)] ~?= 
        (2,0,[[Just Black,Nothing],                                                           [Nothing,Nothing]]),
    "No capture" ~:
        let initialBoard = [[Just Black,Just White],
                            [Just White,Nothing]]
        in captureStones initialBoard Black [(0,0), (1,1)] ~?= (0,0, initialBoard)
    ]

sameColorNeighbors :: Board -> Position -> [Position]
sameColorNeighbors board pos =
  let
    pStone = getStone board pos
    neighbors = neighborPositions' pos
    sameColor = filter (\nPos -> getStone board nPos == pStone) neighbors
  in sameColor

liberties' :: Board -> [Position] -> [Position]
liberties' board positions =
  let
    emptyNeighbors = filter (isEmpty' board) $ concatMap neighborPositions' positions
    sameColorNeighbors' = concatMap (sameColorNeighbors board) positions
  in nub $ emptyNeighbors ++ filter (isEmpty' board) sameColorNeighbors'



captureStone :: Stone -> [Stone] -> Board -> Position -> ([Stone], Board)
captureStone stone captured board pos =
  let
    pStone = getStone board pos
    pNeighbors = isValid' board (neighborPositions' pos)
    hasLiberties = liberties board pNeighbors
    -- pNeighbors = neighborPositions' pos
    -- hasLiberties = liberties board pNeighbors
    (newCaptured, newBoard) = if isJust pStone && pStone /= Just stone && hasLiberties
      then (stone : captured, setNothing board pos)
      else (captured, board)
    (nextCaptured, finalBoard) = foldl (\(ncaptured, nboard) pos ->
      let (nc, nb) = captureStone stone ncaptured nboard pos
      in (nc ++ ncaptured, nb)
      ) (newCaptured, newBoard) (sameColorNeighbors board pos)
  in 
    (nextCaptured, finalBoard)



testCaptureStone :: Test
testCaptureStone = test [
    "Capture single stone with no libeties" ~:
        let initialBoard = [[Just Black,Just White,Nothing],[Just White,Nothing,Just Black],[Just Black,Just White,Nothing]]
            expectedBoard = [[Just Black,Just White,Nothing],[Just White,Nothing,Nothing],[Just Black,Just White,Nothing]]
        in captureStone Black [] initialBoard (1,1) ~?= ([], initialBoard),

    
    "Don't capture anything" ~:
        let initialBoard = [[Just Black,Just White,Nothing],[Just White,Nothing,Just Black],[Just Black,Just White,Nothing]]
        in captureStone Black [] initialBoard (0,0) ~?= ([], initialBoard),

    "capture a ston" ~:
        let initBoard = [[Just Black,Just White,Nothing],[Just White,Nothing,Just Black],[Just Black,Just White,Nothing]]
        in captureStone Black [] initBoard (0,1) ~?= ([Black],[[Just Black,Nothing,Nothing],[Just White,Nothing,Just Black],[Just Black,Just White,Nothing]]),

    "capture a stone white" ~:
        let initBoard = [[Just Black,Just White,Nothing],[Just White,Nothing,Just Black],[Just Black,Just Black,Nothing]]
        in captureStone White [] initBoard (1,2) ~?= ([Black],[[Just Black,Just White,Nothing],[Just White,Nothing,Nothing],[Just Black,Nothing,Nothing]]),

    "Don't capture stone of same color" ~:
        let initialBoard = [[Just Black,Just White,Nothing],[Just White,Nothing,Just Black],[Just Black,Just White,Nothing]]
        in captureStone Black [] initialBoard (0,0) ~?= ([], initialBoard)
    ]

isPositionValid' :: Int -> Position -> Bool
isPositionValid' size (x, y) = x >= 0 && y >= 0 && x < size && y < size

isValid' :: Board -> [Position] -> [Position]
isValid' board positions = filter (isPositionValid' size) positions
  where size = length board
 

isEmpty' :: Board -> Position -> Bool
isEmpty' board pos = isNothing (getStone board pos)--getStone board pos == Nothing

liberties :: Board -> [Position] -> Bool
liberties board = any (isEmpty' board)
--liberties board positions = any (isEmpty' board) positions

testLiberties = test [
  "liberties with empty board and positions" ~:
    liberties emptyBoard [(1,1), (1,2)] @?= True,
  
  "liberties with full board and positions" ~:
    liberties fullBoard [(1,1), (1,2)] @?= False
  ]
  where 
    emptyBoard = replicate 4 (replicate 4 Nothing)
    fullBoard = replicate 4 (replicate 4 (Just Black))


isCapturedGroup :: Board -> Stone -> [Position] -> Bool
isCapturedGroup board stone group =
  let opposite = oppositeStoneG stone
  in all (isCaptured board opposite) group

oppositeStoneG :: Stone -> Stone
oppositeStoneG Black = White
oppositeStoneG White = Black

testOppositeStoneG :: Test
testOppositeStoneG = "oppositeStoneG returns the opposite stone" ~:
  TestList [ oppositeStoneG Black ~?= White
           , oppositeStoneG White ~?= Black
           ]


groupPositions :: Board -> Position -> [Position]
groupPositions board pos =
  let stone = getStone board pos
  in case stone of
    Nothing -> []
    Just s ->
      let
        neighbors = neighborPositions' pos
        connectedNeighbors = filter (\p -> getStone board p == Just s) neighbors
        unvisitedNeighbors = filter (`notElem` [pos] ++ connectedNeighbors) neighbors
        unvisitedGroups = map (groupPositions board) unvisitedNeighbors
      in pos : concat (connectedNeighbors : unvisitedGroups)



setNothing :: Board -> Position -> Board
setNothing board (x,y) = replace x newRow board
  where
    newRow = replace y Nothing (board !! x)

runTests :: IO ()
runTests =  do
  _ <- runTestTT $ TestList [ testInitialBoardLength,
                              testInitialGameState,
                              testShowBoard,
                              testIsOnBoard,
                              testParseMove,
                              testGetStone,
                              testReplace,
                              testSetStone,
                              testSwitchPlayer,
                              testOppositeStone,
                              testNeighborPositions,
                              testOtherPlayer,
                              testBoardPositions,
                              testGetAdjacentPositions,
                              testHasAir,
                              testStones,
                              testIsAdjacent,
                              testTerritories,
                              testGetScore,
                              testCheckGameOver, -- 19
                              testNeighborPositions,
                              testOppositeStoneG,
                              testIsCaptured,
                              testCaptureStone,
                              testCaptureStones,
                              testLiberties,
                              testUpdateBoard,
                              testPlayMove,
                              testGameLoop
                              --testGameLoopTie
                            ]
  return ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
-- liberties :: [(Int, Int)] -> [(Int, Int)]
-- liberties group =
--   let
--     positions = concatMap (neighborPositionsByColor groupBoard (getStone groupBoard (head group))) group
--     groupBoard = replicate boardSize $ replicate boardSize Nothing
--   in
--     filter (\pos -> getStone groupBoard pos == Nothing) positions

-- ---------------------------------------------------------------------------------------------here
-- gameLoop :: GameState -> IO ()
-- gameLoop state = do
--   putStrLn $ "Current player: " ++ show (currentPlayer state)
--   putStrLn $ showBoard $ board state
--   move <- getMove
--   case move of
--     Pass ->
--       if passCount state == 2
--       then do
--         let maybeWinner = checkGameOver state
--         case maybeWinner of
--           Nothing -> putStrLn "The game is a tie."
--           Just winner -> putStrLn $ show winner ++ " wins!"
--       else
--         gameLoop $ state { currentPlayer = togglePlayer $ currentPlayer state, passCount = passCount state + 1 }
--     Resign -> putStrLn $ show (togglePlayer $ currentPlayer state) ++ " wins by resignation!"
--     Move position -> case placeStone (board state) (currentPlayer state) position of
--       Left errorMessage -> do
--         putStrLn $ "Error: " ++ errorMessage
--         gameLoop state
--       Right newBoard -> do
--         let captured = getCapturedStones (board state) newBoard
--             newScore = updateScore (score state) (currentPlayer state) captured
--             newState = GameState
--               { board = newBoard
--               , currentPlayer = togglePlayer $ currentPlayer state
--               , capturedStones = captured
--               , score = newScore
--               , passCount = 0
--               }
--         gameLoop newState

-- getMove :: IO Position
-- getMove = do
--   putStrLn "Enter your move (e.g. '5 5'): "
--   input <- getLine
--   let [x, y] = words input
--   return (read x, read y)

-- checkGameOver :: GameState -> Maybe Stone
-- checkGameOver state =
--   let
--     blackTerritories = territories (board state) Black
--     whiteTerritories = territories (board state) White
--     blackScore = length (filter (\(_, s) -> s == Just Black) (stones (board state))) + length blackTerritories
--     whiteScore = length (filter (\(_, s) -> s == Just White) (stones (board state))) + length whiteTerritories
--     totalStones = length (filter (\(_, s) -> s /= Nothing) (stones (board state)))
--   in
--     if passCount state == 2 || totalStones == boardSize * boardSize
--     then
--       if blackScore > whiteScore then Just Black
--       else if whiteScore > blackScore then Just White
--       else Nothing -- Tie
--     else
--       Nothing



-- -------------------------------------------------------------------------------------------------------------------------------------------------------heree---

-- switchPlayer :: Either String GameState -> Either String GameState
-- switchPlayer (Left errorMsg) = Left errorMsg
-- switchPlayer (Right state) = Right state { currentPlayer = oppositeStone (currentPlayer state) }

-- testSwitchPlayer :: Test
-- testSwitchPlayer =
--   let initialState = Right $ GameState (initialBoard 19) (Just Black) Nothing Nothing 0
--       expectedState = GameState (initialBoard 19) (Just White) Nothing Nothing 0
--       newState = switchPlayer initialState
--   in "switchPlayer switches the current player" ~: Right expectedState ~=? newState
-- testGetNeighborPositions,
                              -- testGroupNeighbors,
                              -- testGroupPositions,
                              -- testIsSame,
                              -- testContainsStone,


-- captureStone :: Stone -> [Stone] -> Board -> Position -> ([Stone], Board)
-- captureStone stone captured board pos =
--   let
--     pStone = getStone board pos
--     pNeighbors = neighborPositions' pos
--     hasLiberties = liberties board pNeighbors
--     (newCaptured, newBoard) = if isJust pStone && pStone /= Just stone &&  hasLiberties
--       then (stone : captured, setNothing board pos)
--       else (captured, board)
--     nextCaptured = foldl (\c pos -> 
--       let (nc, nb) = captureStone stone c newBoard pos
--       in nc ++ c
--       ) newCaptured $ sameColorNeighbors board pos
--   in 
--     (nextCaptured, newBoard)
--------------------------------------------------------------------------
-- captureStones :: Board -> Stone -> [Position] -> (Maybe Int, Board)
-- captureStones board stone positions =
--   let capturedPositions = filter (isCaptured board ( stone)) positions
--       capturedCount = length capturedPositions
--       clearedBoard = foldl' setNothing board capturedPositions
--   in (if capturedCount > 0 then Just capturedCount else Nothing, clearedBoard)
-----------------------------------
--use^^^ this captureStone anBod then use a fucntion to go: 
-- (maybe Int, Board ) -> (mabye Int, Board) -> (maybe Int, maybe Int, Board)
-- combineCaptureStones :: (Maybe Int, Board) -> (Maybe Int, Board) -> (Maybe Int, Maybe Int, Board)
-- combineCaptureStones (blackcount, b) (whitecount, b') =(bc, wc, finalBoard) where
--     bc = blackcount
--     wc = whitecount
--     finalBoard = b

-- captureGroup :: Board -> Stone -> Position -> Maybe (Int, Board)
-- captureGroup board stone pos =
--   let oppositeStone = oppositeStoneG stone
--       positions = groupPositions board pos
--       isCapturedGroup = all (\p -> any (== oppositeStone) (mapMaybe (getStone board) (neighborPositions' p))) positions
--       capturedStones = if isCapturedGroup then positions else []
--       capturedCount = if isCapturedGroup then Just (length capturedStones) else Nothing
--       clearedBoard = foldl' setNothing board capturedStones
--   in if isCapturedGroup then Just (length capturedStones, clearedBoard) else Nothing