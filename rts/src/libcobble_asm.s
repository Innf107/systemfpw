BITS 64

; libcobble.c
extern __prompt_setup_stack

__prompt:
    call __prompt_setup_stack   ; setup the new stack
    mov rbx, [r10]              ; save the stack pointer in the previous prompt
    mov [rbx], rsp              ; ^
    ; we don't need to store the instruction pointer until we are actually yielding (I think?)
    sub r10, 8                  ; decrement the prompt stack pointer (the stack grows downwards!!!)
    mov [r10], rax              ; push the new stack on the prompt stack

    pop rbx                     ; save the return address for __prompt
    mov rsp, rax                ; Actually switch stacks
    jmp rbx                     ; return


__end_prompt:
    mov rsi, rsp        ; save the current stack pointer (the one that is about to be freed) and keep it as an argument for __free_stack
    add r10, 8          ; increment the prompt stack pointer (removing the entry for the current prompt stack)
    mov rsp, [r10+8]    ; restore the previous stack
    jmp __free_stack    ; tailcall into __free_stack TODO: WE PROBABLY NEED TO RESTORE THE PREVIOUS RETURN ADDRESS HERE


__yield:
    pop rax             ; get the return address
    pop rbx             ; get the argument prompt pointer

    mov rcx, [r10]      ; save the current stack pointer

    




