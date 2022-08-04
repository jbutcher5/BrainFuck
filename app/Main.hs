module Main where

import Data.List (foldl')
import Lib

data AsmState = AsmState
  { n :: Int
  , stack :: [Int]
  , acc :: String
  } deriving (Show, Eq)

convert :: Char -> String
convert '+' = inc
convert '-' = dec
convert '>' = moveRight'
convert '<' = moveLeft'
convert '.' = output
convert _ = ""

defaultState :: AsmState
defaultState = AsmState 0 [] ""

generateAsm' :: AsmState -> Char -> AsmState
generateAsm' (AsmState n stack acc) instr = case instr of
  '[' -> AsmState (n + 1) (stack ++ [n]) (acc ++ loopStart n)
  ']' -> AsmState n (tail stack) (acc ++ loopEnd (head stack))
  _ -> AsmState n stack (acc ++ convert instr)

generateAsm :: String -> String
generateAsm input = initial ++ (acc (foldl' generateAsm' defaultState input)) ++ final

main :: IO ()
main = putStrLn . generateAsm $ ">++++++++[<+++++++++>-]<."
  ++ ">++++[<+++++++>-]<+."
  ++ "+++++++.."
  ++ "+++."
  ++ ">>++++++[<+++++++>-]<++."
  ++ "------------."
  ++ ">++++++[<+++++++++>-]<+."
  ++ "<."
  ++ "+++."
  ++ "------."
  ++ "--------."
  ++ ">>>++++[<++++++++>-]<+."
