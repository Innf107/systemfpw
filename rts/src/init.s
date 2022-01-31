BITS 64

global main
extern _main

; libcobble.c
extern allocate_stack

main:
    mov [c_stack], rsp ; save the libc stack

    call allocate_stack ; allocate a larger stack on the heap
    mov rsp, rax        ; replace the program stack
    
    call _main

    pop rax ; return value

    mov rsp, [c_stack] ; restore libc stack
    ret

section .data
    c_stack: dq 1

