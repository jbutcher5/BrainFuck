module Main where

loopStart :: Show a => a -> String
loopStart x = "\n.LOOP_" ++ y ++ ":\n"
  ++ "\n    mov al, [r8]\n"
  ++ "    cmp al, 0\n"
  ++ "    jz .LOOP_" ++ y ++ "_EXIT\n"
  where y = show x

loopEnd :: Show a => a -> String
loopEnd x = "\n    mov al, [r8]\n"
  ++ "    cmp al, 0\n"
  ++ "    jnz .LOOP_" ++ y ++ "\n"
  ++ "\n.LOOP_" ++ y ++ "_EXIT:\n"
  where y = show x

initial :: String
initial = "section .text\n"
  ++ "   global _start\n\n"
  ++ "_start:\n"
  ++ "    mov r8, msg\n"

final :: String
final = "\n    mov rax, 60\n"
  ++ "    mov rdi, 0\n"
  ++ "    syscall\n\n"
  ++ "section .bss\n"
  ++ "    msg:    resb 1024"

output :: String
output = "\n    mov rax, 1\n"
  ++ "    mov rdi, 1\n"
  ++ "    mov rsi, r8\n"
  ++ "    mov rdx, 1\n"
  ++ "    syscall\n"

inc :: String
inc = "\n    mov al, [r8]\n"
  ++ "    inc al\n"
  ++ "    mov [r8], al\n"

dec :: String
dec = "\n    mov al, [r8]\n"
  ++ "    dec al\n"
  ++ "    mov [r8], al\n"

moveRight' :: String
moveRight' = "\n    inc r8\n"

moveLeft' :: String
moveLeft' = "\n    dec r8\n"

generateAsm' :: String -> Int -> [Int] -> String -> String
generateAsm' ('+':xs) n stack acc = generateAsm' xs n stack (acc ++ inc)
generateAsm' ('-':xs) n stack acc = generateAsm' xs n stack (acc ++ dec)
generateAsm' ('<':xs) n stack acc = generateAsm' xs n stack (acc ++ moveRight')
generateAsm' ('>':xs) n stack acc = generateAsm' xs n stack (acc ++ moveLeft')
generateAsm' ('.':xs) n stack acc = generateAsm' xs n stack (acc ++ output)
generateAsm' ('[':xs) n stack acc = generateAsm' xs (n + 1) (stack ++ [n]) (acc ++ loopStart n)
generateAsm' (']':xs) n (x:xs') acc = generateAsm' xs n xs' (acc ++ loopEnd x)
generateAsm' (_:xs) n stack acc = generateAsm' xs n stack acc
generateAsm' "" _ _ result = result

generateAsm :: String -> String
generateAsm input = initial ++ generateAsm' input 0 [] ("" ++ final)

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
