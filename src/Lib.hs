module Lib where

macros :: String
macros = "\n%macro offset 1\n"
  ++ "    mov al, [r8]\n"
  ++ "    add al, %1\n"
  ++ "    mov [r8], al\n"
  ++ "%endmacro\n\n"

  ++ "%macro move 1\n"
  ++ "    mov rax, r8\n"
  ++ "    add rax, %1\n"
  ++ "    mov r8, rax\n"
  ++ "%endmacro\n\n"

  ++ "%macro io 1\n"
  ++ "    mov rax, %1\n"
  ++ "    mov rdi, %1\n"
  ++ "    mov rsi, r8\n"
  ++ "    mov rdx, 1\n"
  ++ "    syscall\n"
  ++ "%endmacro\n\n"

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
initial = macros ++ "section .text\n"
  ++ "   global _start\n\n"
  ++ "_start:\n"
  ++ "    mov r8, msg\n"

final :: String
final = "\n    mov rax, 60\n"
  ++ "    mov rdi, 0\n"
  ++ "    syscall\n\n"
  ++ "section .bss\n"
  ++ "    msg:    resb 1024"

composeAsm :: String -> Int -> String
composeAsm x y = "\n    " ++ x ++ " " ++ show y ++ "\n"

offset :: Int -> String
offset = composeAsm "offset"

io :: Int -> String
io = composeAsm "io"

move :: Int -> String
move = composeAsm "move"
