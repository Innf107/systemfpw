#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

const size_t stack_size = 4*1024*1024;

void* __allocate_stack(){

    void* stack_end = mmap(0, stack_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    return stack_end + stack_size; // The stack grows downwards
}

void __free_stack(void* stackPtr){
    int success = munmap(stackPtr - stack_size, stack_size);
    if (success != 0){
        printf("panic @__free_stack: munmap exited with code: %d\n", errno);
        exit(1);
    }
}

void __panic (const char* msg){
    printf(msg);
    exit(1);
}

// Called by the asm function __prompt, which is called by the 'Prompt' stack machine instruction.
// This function allocates a new stack, setting appropriate metadata and pushing
// the prompt marker.
// Refer to [docs/StackLayout.md] for the exact layout of the new Stack.
void** __prompt_setup_stack(void* yieldAddress, void* returnAddress){
    void** stack = __allocate_stack();
    // IMPORTANT: The stack grows downwards!
    // Set the stack pointer. This includes the pushed prompt marker.
    stack[0] = stack[-5];
    // The instruction pointer remains uninitialized until control switches to a different stack (yield is called)
    // Set the yield address. This one is called by `yield` and thus will not change over the prompt lifetime
    stack[-2] = yieldAddress;
    // Set the return address. This one is called when the prompt is exited normally (without yielding). It will not change over time.
    stack[-3] = returnAddress;
    // Push the prompt marker (a pointer to the stack base) 
    stack[-4] = stack;

    return stack;
}



