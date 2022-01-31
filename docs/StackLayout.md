# Stack Layout

Each prompt receives its own stack.

```
+---+---------------------------+
|8  | stack pointer             | Header
|8  | instruction pointer       |
|8  | yield address             |
|8  | return address            |
+---+---------------------------+
|8  | prompt marker             | Stack Content
|   | ...                       |
+---+---------------------------+
                  |
                  V

``` 

r10 contains the stack pointer for the prompt stack, which includes markers for previous prompts that will be returned to after 'Prompt' is finished. The current prompt stack is always located at [r10].


