module Main where

loopStart :: Show a => a -> String
loopStart x = "\n.LOOP_" ++ y ++ ":\n"
  ++ "    mov al, [r8]\n"
  ++ "    cmp al, 0\n"
  ++ "    jz .LOOP_" ++ y ++ "_EXIT"
  where y = show x

loopEnd :: Show a => a -> String
loopEnd x = "\n    mov al, [r8]\n"
  ++ "    cmp al, 0\n"
  ++ "    jz .LOOP_" ++ y ++ "\n"
  ++ ".LOOP_" ++ y ++ "_EXIT:\n"
  where y = show x

initial :: String
initial = "section .text\n\
              \   global _start\n\n\
          \_start:\n\
          \    mov r8, msg\n"

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
         \    mov rdx, 1\n\
         \    syscall\n"

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
