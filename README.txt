Peter Hess
A8 - Compiler in Ocaml - Nov 2018


In order to turn the translator into a compiler, we must replace all function definitions (except main) with labels and their corresponding function calls, with a goto that label. For each function, the local array must be eliminated and instead allocated as part of the global mem array (the stack). Accesses to the local array must be switched to accesses of the stack. 

To achieve this, the caller must first push all parameters needed by the callee onto the top of the stack, being sure to update the sp register. The caller then stores the current frame pointer on top of the stack (mem[sp] = fp) before pushing the label of the return location, while leaving space for the return value of the callee. Finally, the caller jumps to the callee label. Once called, the callee must initialize its stack frame by updating fp to the current value of sp, then adding the local array size to sp.

The callee then executes the body of its code. Before returning, the callee must store its return value on the stack (at mem[fp-1]), restore the sp (sp = fp), then jump to the return address label (stored at mem[fp-2]). Finally, upon reentry into the caller, the return value of the callee is copied into the desired location on the stack. Then the frame pointer and stack pointer are updated (fp = mem[sp-3] and sp = base + size of local array of caller). 