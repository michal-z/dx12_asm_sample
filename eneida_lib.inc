;=============================================================================
; in: rcx - addr of zero-terminated file name
; out: rax - addr of loaded data, edx - size of loaded data (in bytes)
falign
load_file:
;-----------------------------------------------------------------------------
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
; in: rcx - buffer size
; out: rax - buffer resource, rdx - cpu buffer address
falign
create_upload_buffer:
;-----------------------------------------------------------------------------
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 8 n { .param#n dq ? }
  .buffer dq ?
  .addr dq ?
  dalign 8
  .upload_heap_props D3D12_HEAP_PROPERTIES
  dalign 8
  .buffer_res_desc D3D12_RESOURCE_DESC
  dalign 8
  .empty_range D3D12_RANGE
  dalign 32
  .k_stack_size = $-$$+24
  end virtual
        $zeroStack .k_stack_size

        $mov [.upload_heap_props.Type], D3D12_HEAP_TYPE_UPLOAD
        $mov [.buffer_res_desc.Dimension], D3D12_RESOURCE_DIMENSION_BUFFER
        $mov [.buffer_res_desc.Width], rcx
        $mov [.buffer_res_desc.Height], 1
        $mov [.buffer_res_desc.DepthOrArraySize], 1
        $mov [.buffer_res_desc.MipLevels], 1
        $mov [.buffer_res_desc.SampleDesc.Count], 1
        $mov [.buffer_res_desc.Layout], D3D12_TEXTURE_LAYOUT_ROW_MAJOR

        ; create buffer
        $mov rcx, [glob.device]
        $lea rdx, [.upload_heap_props]
        $mov r8d, D3D12_HEAP_FLAG_NONE
        $lea r9, [.buffer_res_desc]
        $mov [.param5], D3D12_RESOURCE_STATE_GENERIC_READ
        $mov [.param6], 0
        $lea [.param7], rax, [IID_ID3D12Resource]
        $lea [.param8], rax, [.buffer]
        $comcall ID3D12Device.CreateCommittedResource
        $checkhr eax, .error

        ; map buffer
        $mov rcx, [.buffer]
        $xor edx, edx
        $lea r8, [.empty_range]
        $lea r9, [.addr]
        $comcall ID3D12Resource.Map
        $checkhr eax, .error

        $mov rax, [.buffer]
        $mov rdx, [.addr]
        $jmp .return
  .error:
        $xor eax, eax
        $xor edx, edx
  .return:
        $add rsp, .k_stack_size
        $ret
;=============================================================================