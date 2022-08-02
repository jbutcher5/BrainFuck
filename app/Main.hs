module Main where

initial :: String
initial = "section .text\n\
              \   global _start\n\n\
          \_start:\n\
          \    mov r8, msg\n\
          \    mov al, 0\n\
          \    mov [r8], al\n"

final :: String
final = "\n    mov rax, 60\n\
        \    mov rdi, 0\n\
        \    syscall\n\n\
        \section .bss\n\
        \    msg:    resb 1024"

output :: String
output = "\n    mov rax, 1\n\
         \    mov rdi, 1\n\
         \    mov rsi, r8\n\
         \    mov rdi, 1\n"

inc :: String
inc = "\n    mov al, [r8]\n\
      \    inc al\n\
      \    mov [r8], al\n"

dec :: String
dec = "\n    mov al, [r8]\n\
      \    dec al\n\
      \    mov [r8], al\n"

moveright :: String
moveright = "\n    inc r8\n"

moveleft :: String
moveleft = "\n    dec r8\n"

resolveToken :: Char -> String
resolveToken '+' = inc
resolveToken '-' = dec
resolveToken '>' = moveright
resolveToken '<' = moveleft
resolveToken '.' = output
resolveToken x = [x]

f :: Char -> String -> String
f x y = resolveToken x ++ y

generateAsm :: String -> String
generateAsm input = initial ++ foldr f "" input ++ final

main :: IO ()
main = putStrLn $ generateAsm "+."
