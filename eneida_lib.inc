;========================================================================
; in: rcx - addr of zero-terminated file name
; out: rax - addr of loaded data, edx - size of loaded data (in bytes)
falign
load_file:
;------------------------------------------------------------------------
  virtual at rsp
  rept 7 n { .param#n dq ? }
  .bytes_read dd ?
  dalign 32
  .k_stack_size = $-$$
  end virtual
        $push rdi rsi rbx
        $sub rsp, .k_stack_size
        $xor esi, esi                                   ; rsi - file handle
        $xor edi, edi                                   ; rdi - memory pointer

        $mov edx, GENERIC_READ
        $xor r8d, r8d
        $xor r9d, r9d
        $mov [.param5], OPEN_EXISTING
        $mov [.param6], FILE_ATTRIBUTE_NORMAL
        $mov [.param7], 0
        $icall CreateFile
        $cmp rax, INVALID_HANDLE_VALUE
        $je .error

        $mov rsi, rax
        $mov rcx, rsi
        $xor edx, edx
        $icall GetFileSize
        $cmp eax, INVALID_FILE_SIZE
        $je .error

        $mov ebx, eax
        $malloc ebx
        $test rax, rax
        $jz .error
        $mov rdi, rax

        $mov rcx, rsi
        $mov rdx, rdi
        $mov r8d, ebx
        $lea r9, [.bytes_read]
        $mov [.param5], 0
        $icall ReadFile
        $test eax, eax
        $jz .error
        $cmp [.bytes_read], ebx
        $jne .error

        $mov rcx, rsi
        $icall CloseHandle
        $mov rax, rdi
        $mov edx, ebx
        $jmp .return
  .error:
        $safeClose rsi
        $free rdi
        $xor eax, eax
        $xor edx, edx
  .return:
        $add rsp, .k_stack_size
        $pop rbx rsi rdi
        $ret
;=============================================================================
