BITS 64

global main

extern malloc

_main:
    jmp main
    ret

main:
    mov rdi, 300_000_000    ; allocate large stack
    call malloc             ; ^

    add rax, 300_000_000    ; adjust stack pointer (the stack grows downwards)

    mov [c_stack], rsp      ; save the previous stack for libc
    mov rsp, rax            ; replace the stack pointer

    call _main              ; call the actual code

    mov rsp, [c_stack]      ; restore libc stack
    ret

section .data
    handlers: dq 100
    c_stack: dq 1

