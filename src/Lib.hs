module Lib where

macros :: String
macros = "%macro offset 1\n"
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

  ++ "%macro loop_start 1\n"
  ++ ".LOOP_%1:"
  ++ "    mov al, [r8]\n"
  ++ "    cmp al, 0\n"
  ++ "    jz .LOOP_%1_EXIT\n"
  ++ "%endmacro\n\n"

  ++ "%macro loop_end 1\n"
  ++ "    mov al, [r8]\n"
  ++ "    cmp al, 0\n"
  ++ "    jnz .LOOP_%1\n"
  ++ ".LOOP_%1_EXIT:\n"
  ++ "%endmacro\n\n"

initial :: String
initial = macros ++ "section .text\n"
  ++ "   global _start\n\n"
  ++ "_start:\n"
  ++ "    mov r8, msg\n"

final :: String
final = "\n\n    mov rax, 60\n"
  ++ "    mov rdi, 0\n"
  ++ "    syscall\n\n"
  ++ "section .bss\n"
  ++ "    msg:    resb 1024"

composeAsm :: Show a => String -> a -> String
composeAsm "" _ = ""
composeAsm x y = "\n    " ++ x ++ " " ++ show y
