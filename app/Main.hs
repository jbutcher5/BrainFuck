module Main where

import Lib

generateAsm' :: String -> Int -> [Int] -> String -> String
generateAsm' ('+':xs) n stack acc = generateAsm' xs n stack $ acc ++ inc
generateAsm' ('-':xs) n stack acc = generateAsm' xs n stack $ acc ++ dec
generateAsm' ('<':xs) n stack acc = generateAsm' xs n stack $ acc ++ moveLeft'
generateAsm' ('>':xs) n stack acc = generateAsm' xs n stack $ acc ++ moveRight'
generateAsm' ('.':xs) n stack acc = generateAsm' xs n stack $ acc ++ output
generateAsm' ('[':xs) n stack acc = generateAsm' xs (n + 1) (stack ++ [n]) (acc ++ loopStart n)
generateAsm' (']':xs) n (x:xs') acc = generateAsm' xs n xs' $ acc ++ loopEnd x
generateAsm' (_:xs) n stack acc = generateAsm' xs n stack acc
generateAsm' "" _ _ result = result

generateAsm :: String -> String
generateAsm input = initial ++ generateAsm' input 0 [] "" ++ final

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
