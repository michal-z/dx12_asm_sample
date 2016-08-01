;=============================================================================
; in: rcx - addr of zero-terminated file name
; out: rax - addr of loaded data, edx - size of loaded data (in bytes)
falign
load_file:
;-----------------------------------------------------------------------------
            push        rdi rsi rbx
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 7 n { .param#n dq ? }

  .bytes_read dd ?

  dalign 32, .k_stack_size = $-$$
  end virtual
  ;---------------------------------------
            xor         esi, esi                                   ; rsi - file handle
            xor         edi, edi                                   ; rdi - memory pointer

            mov         edx, GENERIC_READ
            xor         r8d, r8d
            xor         r9d, r9d
            mov         [.param5], OPEN_EXISTING
            mov         [.param6], FILE_ATTRIBUTE_NORMAL
            mov         [.param7], 0
            icall       CreateFile
            cmp         rax, INVALID_HANDLE_VALUE
            je          .error

            mov         rsi, rax
            mov         rcx, rsi
            xor         edx, edx
            icall       GetFileSize
            cmp         eax, INVALID_FILE_SIZE
            je          .error

            mov         ebx, eax
            malloc      ebx
            test        rax, rax
            jz          .error
            mov         rdi, rax

            mov         rcx, rsi
            mov         rdx, rdi
            mov         r8d, ebx
            lea         r9, [.bytes_read]
            mov         [.param5], 0
            icall       ReadFile
            test        eax, eax
            jz          .error
            cmp         [.bytes_read], ebx
            jne         .error

            mov         rcx, rsi
            icall       CloseHandle
            mov         rax, rdi
            mov         edx, ebx
            jmp         .return
  .error:   safeClose   rsi
            free        rdi
            xor         eax, eax
            xor         edx, edx
  .return:  add         rsp, .k_stack_size
            pop         rbx rsi rdi
            ret
;=============================================================================
; out: xmm0 - current time since application start in seconds
falign
get_time:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
            sub         rsp, .k_stack_size
            mov         rax, [.perf_freq]
            test        rax, rax
            jnz         @f

            lea         rcx, [.perf_freq]
            icall       QueryPerformanceFrequency
            lea         rcx, [.first_perf_counter]
            icall       QueryPerformanceCounter

  @@:       lea         rcx, [.perf_counter]
            icall       QueryPerformanceCounter
            mov         rcx, [.perf_counter]
            sub         rcx, [.first_perf_counter]
            mov         rdx, [.perf_freq]
            vxorps      xmm0, xmm0, xmm0
            vcvtsi2sd   xmm1, xmm0, rcx
            vcvtsi2sd   xmm2, xmm0, rdx
            vdivsd      xmm0, xmm1, xmm2
            add         rsp, .k_stack_size
            ret
;=============================================================================
; in: rcx - buffer size, edx - D3D12_HEAP_TYPE_*, r8d - D3D12_RESOURCE_STATE_*
; out: rax - buffer resource
falign
create_buffer:
;-----------------------------------------------------------------------------
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 8 n { .param#n dq ? }

  .buffer dq ?

  dalign 8, .upload_heap_props D3D12_HEAP_PROPERTIES
  dalign 8, .buffer_res_desc   D3D12_RESOURCE_DESC

  dalign 32, .k_stack_size = $-$$+24
  end virtual
  ;---------------------------------------
            zeroStack   .k_stack_size

            mov         [.upload_heap_props.Type], edx             ; D3D12_HEAP_TYPE_*
            mov         [.buffer_res_desc.Dimension], D3D12_RESOURCE_DIMENSION_BUFFER
            mov         [.buffer_res_desc.Width], rcx              ; buffer size
            mov         [.buffer_res_desc.Height], 1
            mov         [.buffer_res_desc.DepthOrArraySize], 1
            mov         [.buffer_res_desc.MipLevels], 1
            mov         [.buffer_res_desc.SampleDesc.Count], 1
            mov         [.buffer_res_desc.Layout], D3D12_TEXTURE_LAYOUT_ROW_MAJOR

            ; create buffer
            mov         dword[.param5], r8d                        ; D3D12_RESOURCE_STATE_*
            mov         rcx, [glob.device]
            lea         rdx, [.upload_heap_props]
            mov         r8d, D3D12_HEAP_FLAG_NONE
            lea         r9, [.buffer_res_desc]
            mov         [.param6], 0
            lea         [.param7], rax, [IID_ID3D12Resource]
            lea         [.param8], rax, [.buffer]
            comcall     ID3D12Device.CreateCommittedResource
            checkhr     eax, .error

            mov         rax, [.buffer]
            jmp         .return
  .error:   xor         eax, eax
  .return:  add         rsp, .k_stack_size
            ret
;=============================================================================
; in: rcx - buffer size
; out: rax - buffer resource, rdx - cpu buffer address
falign
create_upload_buffer:
;-----------------------------------------------------------------------------
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 4 n { .param#n dq ? }

  .buffer      dq ?
  .addr        dq ?

  dalign 8, .empty_range D3D12_RANGE

  dalign 32, .k_stack_size = $-$$+24
  end virtual
  ;---------------------------------------
            zeroStack   .k_stack_size

            mov         edx, D3D12_HEAP_TYPE_UPLOAD
            mov         r8d, D3D12_RESOURCE_STATE_GENERIC_READ
            call        create_buffer
            mov         [.buffer], rax
            test        rax, rax
            jz          .error

            ; map buffer
            mov         rcx, [.buffer]
            xor         edx, edx
            lea         r8, [.empty_range]
            lea         r9, [.addr]
            comcall     ID3D12Resource.Map
            checkhr     eax, .error

            mov         rax, [.buffer]
            mov         rdx, [.addr]
            jmp         .return

  .error:   safeRelease [.buffer]
            xor         eax, eax
            xor         edx, edx
  .return:  add         rsp, .k_stack_size
            ret
;=============================================================================
falign
wait_for_gpu:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
            sub         rsp, .k_stack_size
            add         [glob.cpu_completed_fences], 1

            mov         rcx, [glob.cmdqueue]
            mov         rdx, [glob.frame_fence]
            mov         r8, [glob.cpu_completed_fences]
            comcall     ID3D12CommandQueue.Signal

            mov         rcx, [glob.frame_fence]
            mov         rdx, [glob.cpu_completed_fences]
            mov         r8, [glob.frame_fence_event]
            comcall     ID3D12Fence.SetEventOnCompletion

            mov         rcx, [glob.frame_fence_event]
            mov         edx, INFINITE
            icall       WaitForSingleObject

            add         rsp, .k_stack_size
            ret
;=============================================================================
; in: rcx - buffer size
; out: rax - buffer resource, rdx - cpu buffer address
falign
get_upload_buffer:
;-----------------------------------------------------------------------------
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 4 n { .param#n dq ? }

  .index dd ?, ?

  dalign 32, .k_stack_size = $-$$+24
  end virtual
  ;---------------------------------------
            mov         eax, 1
            lock xadd   [glob.upload_buffers_count], eax
            cmp         eax, k_upload_buffers_count
            jae         .error
            mov         [.index], eax

            call        create_upload_buffer
            test        rax, rax
            jz          .error
            mov         r8d, [.index]
            mov         [glob.upload_buffers+r8*8], rax
            jmp         .return

  .error:   xor         eax, eax
            xor         edx, edx
  .return:  add         rsp, .k_stack_size
            ret
;=============================================================================
; in: rcx - addr of zero-terminated file name
; out: rax - texture 2D resource
falign
create_texture_from_file:
;-----------------------------------------------------------------------------
            push        rsi rdi rbx
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 9 n { .param#n dq ? }

  .texture               dq ?
  .upload_buffer         dq ?
  .row_count             dd ?, ?
  .row_size              dq ?
  .buffer_size           dq ?
  .image_width           dd ?
  .image_height          dd ?
  .image_addr            dq ?

  dalign 8, .copy_dst          D3D12_TEXTURE_COPY_LOCATION
  dalign 8, .copy_src          D3D12_TEXTURE_COPY_LOCATION
  dalign 8, .barrier           D3D12_RESOURCE_BARRIER
  dalign 8, .upload_heap_props D3D12_HEAP_PROPERTIES
  dalign 8, .texture_desc      D3D12_RESOURCE_DESC
  dalign 8, .layout            D3D12_PLACED_SUBRESOURCE_FOOTPRINT

  dalign 32, .k_stack_size = $-$$
  end virtual
  ;---------------------------------------
            zeroStack   .k_stack_size

            ; init transition barriers
            mov         [.barrier.Type], D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
            mov         [.barrier.Transition.Subresource], D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES


            ; load image data from file (file name addr in rcx, only BGRA8 TGA are supported)
            call        load_file
            movzx       ecx, word[rax+12]
            mov         [.image_width], ecx
            movzx       ecx, word[rax+14]
            mov         [.image_height], ecx
            add         rax, 18
            mov         [.image_addr], rax


            mov         [.upload_heap_props.Type], D3D12_HEAP_TYPE_DEFAULT
            mov         [.texture_desc.Dimension], D3D12_RESOURCE_DIMENSION_TEXTURE2D
            mov         dword[.texture_desc.Width], eax, [.image_width]
            mov         [.texture_desc.Height], eax, [.image_height]
            mov         [.texture_desc.DepthOrArraySize], 1
            mov         [.texture_desc.MipLevels], 0
            mov         [.texture_desc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
            mov         [.texture_desc.SampleDesc.Count], 1
            mov         [.texture_desc.Layout], D3D12_TEXTURE_LAYOUT_UNKNOWN

            mov         rcx, [glob.device]
            lea         rdx, [.texture_desc]
            mov         r8d, 0                             ; subresource index
            mov         r9d, 1                             ; number of subresources
            mov         [.param5], 0                       ; base offset
            lea         [.param6], rax, [.layout]
            lea         [.param7], rax, [.row_count]
            lea         [.param8], rax, [.row_size]
            lea         [.param9], rax, [.buffer_size]
            comcall     ID3D12Device.GetCopyableFootprints


            ; copy image content to upload buffer
            mov         rcx, [.buffer_size]
            call        get_upload_buffer
            mov         [.upload_buffer], rax

            mov         rsi, [.image_addr]
            mov         rdi, rdx                   ; upload buffer addr
            mov         ebx, [.row_count]
  .copy_loop:
            mov         rcx, rdi
            mov         rdx, rsi
            mov         r8, [.row_size]
            icall       RtlCopyMemory
            mov         eax, [.layout.Footprint.RowPitch]
            add         rdi, rax
            add         rsi, [.row_size]
            sub         ebx, 1
            jnz         .copy_loop

            ; free allocated memory
            mov         rax, [.image_addr]
            sub         rax, 18
            free        rax

            ; create texture
            mov         rcx, [glob.device]
            lea         rdx, [.upload_heap_props]
            mov         r8d, D3D12_HEAP_FLAG_NONE
            lea         r9, [.texture_desc]
            mov         [.param5], D3D12_RESOURCE_STATE_COPY_DEST
            mov         [.param6], 0
            lea         [.param7], rax, [IID_ID3D12Resource]
            lea         [.param8], rax, [.texture]
            comcall     ID3D12Device.CreateCommittedResource
            checkhr     eax, .error


            mov         [.copy_dst.pResource], rax, [.texture]
            mov         [.copy_dst.Type], eax, D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX
            mov         [.copy_dst.SubresourceIndex], 0

            mov         [.copy_src.pResource], rax, [.upload_buffer]
            mov         [.copy_src.Type], eax, D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT
            mov         [.copy_src.PlacedFootprint.Offset], 0
            mov         [.copy_src.PlacedFootprint.Footprint.Format], eax, [.layout.Footprint.Format]
            mov         [.copy_src.PlacedFootprint.Footprint.Width], eax, [.layout.Footprint.Width]
            mov         [.copy_src.PlacedFootprint.Footprint.Height], eax, [.layout.Footprint.Height]
            mov         [.copy_src.PlacedFootprint.Footprint.Depth], eax, [.layout.Footprint.Depth]
            mov         [.copy_src.PlacedFootprint.Footprint.RowPitch], eax, [.layout.Footprint.RowPitch]


            mov         rcx, [glob.cmdlist]
            lea         rdx, [.copy_dst]
            mov         r8d, 0
            mov         r9d, 0
            mov         [.param5], 0
            lea         [.param6], rax, [.copy_src]
            mov         [.param7], 0
            comcall     ID3D12GraphicsCommandList.CopyTextureRegion

            transitionBarrier .barrier, [.texture], D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE

            mov         rcx, [glob.cmdlist]
            mov         edx, 1
            lea         r8, [.barrier]
            comcall     ID3D12GraphicsCommandList.ResourceBarrier


            mov         rax, [.texture]
            jmp         .return
  .error:   xor         eax, eax
  .return:  add         rsp, .k_stack_size
            pop         rbx rdi rsi
            ret
;=============================================================================
; vim: ft=fasm ts=12 sts=12 sw=12 autoindent :
