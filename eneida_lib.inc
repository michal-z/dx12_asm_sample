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
; out: xmm0 - current time since application start in seconds
falign
get_time:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
        $sub rsp, .k_stack_size
        $mov rax, [.perf_freq]
        $test rax, rax
        $jnz @f

        $lea rcx, [.perf_freq]
        $icall QueryPerformanceFrequency
        $lea rcx, [.first_perf_counter]
        $icall QueryPerformanceCounter

  @@:   $lea rcx, [.perf_counter]
        $icall QueryPerformanceCounter
        $mov rcx, [.perf_counter]
        $sub rcx, [.first_perf_counter]
        $mov rdx, [.perf_freq]
        $vxorps xmm0, xmm0, xmm0
        $vcvtsi2sd xmm1, xmm0, rcx
        $vcvtsi2sd xmm2, xmm0, rdx
        $vdivsd xmm0, xmm1, xmm2
        $add rsp, .k_stack_size
        $ret
;=============================================================================
; in: rcx - buffer size, edx - D3D12_HEAP_TYPE_*, r8d - D3D12_RESOURCE_STATE_*
; out: rax - buffer resource
falign
create_buffer:
;-----------------------------------------------------------------------------
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 8 n { .param#n dq ? }
  .buffer dq ?
  dalign 8
  .upload_heap_props D3D12_HEAP_PROPERTIES
  dalign 8
  .buffer_res_desc D3D12_RESOURCE_DESC
  dalign 32
  .k_stack_size = $-$$+24
  end virtual
        $zeroStack .k_stack_size

        $mov [.upload_heap_props.Type], edx             ; D3D12_HEAP_TYPE_*
        $mov [.buffer_res_desc.Dimension], D3D12_RESOURCE_DIMENSION_BUFFER
        $mov [.buffer_res_desc.Width], rcx              ; buffer size
        $mov [.buffer_res_desc.Height], 1
        $mov [.buffer_res_desc.DepthOrArraySize], 1
        $mov [.buffer_res_desc.MipLevels], 1
        $mov [.buffer_res_desc.SampleDesc.Count], 1
        $mov [.buffer_res_desc.Layout], D3D12_TEXTURE_LAYOUT_ROW_MAJOR

        ; create buffer
        $mov dword[.param5], r8d                        ; D3D12_RESOURCE_STATE_*
        $mov rcx, [glob.device]
        $lea rdx, [.upload_heap_props]
        $mov r8d, D3D12_HEAP_FLAG_NONE
        $lea r9, [.buffer_res_desc]
        $mov [.param6], 0
        $lea [.param7], rax, [IID_ID3D12Resource]
        $lea [.param8], rax, [.buffer]
        $comcall ID3D12Device.CreateCommittedResource
        $checkhr eax, .error

        $mov rax, [.buffer]
        $jmp .return
  .error:
        $xor eax, eax
  .return:
        $add rsp, .k_stack_size
        $ret
;=============================================================================
; in: rcx - buffer size
; out: rax - buffer resource, rdx - cpu buffer address
falign
create_upload_buffer:
;-----------------------------------------------------------------------------
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 4 n { .param#n dq ? }
  .buffer dq ?
  .addr dq ?
  .empty_range D3D12_RANGE
  dalign 32
  .k_stack_size = $-$$+24
  end virtual
        $zeroStack .k_stack_size

        $mov edx, D3D12_HEAP_TYPE_UPLOAD
        $mov r8d, D3D12_RESOURCE_STATE_GENERIC_READ
        $call create_buffer
        $mov [.buffer], rax
        $test rax, rax
        $jz .error

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
        $safeRelease [.buffer]
        $xor eax, eax
        $xor edx, edx
  .return:
        $add rsp, .k_stack_size
        $ret
;=============================================================================
falign
wait_for_gpu:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
        $sub rsp, .k_stack_size

        $add [glob.cpu_completed_fences], 1

        $mov rcx, [glob.cmdqueue]
        $mov rdx, [glob.frame_fence]
        $mov r8, [glob.cpu_completed_fences]
        $comcall ID3D12CommandQueue.Signal

        $mov rcx, [glob.frame_fence]
        $mov rdx, [glob.cpu_completed_fences]
        $mov r8, [glob.frame_fence_event]
        $comcall ID3D12Fence.SetEventOnCompletion

        $mov rcx, [glob.frame_fence_event]
        $mov edx, INFINITE
        $icall WaitForSingleObject

        $add rsp, .k_stack_size
        $ret
;=============================================================================
; in: rcx - buffer size
; out: rax - buffer resource, rdx - cpu buffer address
falign
get_upload_buffer:
;-----------------------------------------------------------------------------
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 4 n { .param#n dq ? }
  .index dd ?, ?
  dalign 32
  .k_stack_size = $-$$+24
  end virtual

        $mov eax, 1
        $lock $xadd [glob.upload_buffers_count], eax
        $cmp eax, k_upload_buffers_count
        $jae .error
        $mov [.index], eax

        $call create_upload_buffer
        $test rax, rax
        $jz .error
        $mov r8d, [.index]
        $mov [glob.upload_buffers+r8*8], rax
        $jmp .return

  .error:
        $xor eax, eax
        $xor edx, edx
  .return:
        $add rsp, .k_stack_size
        $ret
;=============================================================================
