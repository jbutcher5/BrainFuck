{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (foldl')
import System.Environment (getArgs)
import Distribution.Simple.Utils (safeHead)

import Lib
  ( composeAsm
  , initial
  , final
  )

data Instr
  = Move Int
  | Offset Int
  | IO Int
  | LoopStart
  | LoopEnd
  | Void deriving (Show, Eq)

data AsmState = AsmState
  { n :: Int
  , stack :: [Int]
  , acc :: String
  } deriving (Show, Eq)

translate :: Char -> Instr
translate = \case
  '+' -> Offset 1
  '-' -> Offset $ -1
  '>' -> Move 1
  '<' -> Move $ -1
  '.' -> IO 1
  ',' -> IO 0
  '[' -> LoopStart
  ']' -> LoopEnd
  _ -> Void

convert :: Instr -> (String, Int)
convert = \case
  (Offset x) -> ("offset", x)
  (Move x) -> ("move", x)
  (IO x) -> ("io", x)
  _ -> ("", 0)

mergeOperators :: [Instr] -> Instr -> [Instr]
mergeOperators ((Offset y):rest) (Offset x) = Offset (y + x):rest
mergeOperators ((Move y):rest) (Move x) = Move (y + x):rest
mergeOperators acc x = x:acc

mapInstructions :: String -> [Instr]
mapInstructions input = foldl' mergeOperators [] (map translate input)

defaultState :: AsmState
defaultState = AsmState 0 [] ""

generateAsm' :: AsmState -> Instr -> AsmState
generateAsm' (AsmState n stack acc) instr = case instr of
  LoopStart -> AsmState (n + 1) (stack ++ [n]) (acc ++ composeAsm "loop_start" n)
  LoopEnd -> AsmState n (tail stack) (acc ++ composeAsm "loop_end" (head stack))
  _ -> AsmState n stack (acc ++ uncurry composeAsm (convert instr))

generateAsm :: String -> String
generateAsm input = acc $ foldl' generateAsm' defaultState $ reverse $ mapInstructions input

main :: IO ()
main = do
  args <- getArgs

  case safeHead args of
    Just path -> do
      content <- readFile path
      putStrLn $ initial ++ generateAsm content ++ final

    Nothing -> putStrLn "An input file is required."
