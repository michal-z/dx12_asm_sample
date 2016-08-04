;=============================================================================
falign
demo_init:
;-----------------------------------------------------------------------------
            push        rsi rbx
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 7 n { .param#n dq ? }

  .dbgi      dq ?
  .swapchain dq ?

  dalign 8, .cmdqueue_desc        D3D12_COMMAND_QUEUE_DESC
  dalign 8, .swapchain_desc       DXGI_SWAP_CHAIN_DESC
  dalign 8, .rtv_heap_desc        D3D12_DESCRIPTOR_HEAP_DESC
  dalign 8, .descriptor_heap_desc D3D12_DESCRIPTOR_HEAP_DESC

  dalign 32, .k_stack_size = $-$$+8
  end virtual

  virtual at rbx
  .fres frame_resources
  end virtual
  ;---------------------------------------
            zeroStack   .k_stack_size
            ;
            ; Debug layer
            ;
            lea         rcx, [IID_ID3D12Debug]
            lea         rdx, [.dbgi]
            icall       D3D12GetDebugInterface
            checkhr     eax, @f
            mov         rcx, [.dbgi]
            comcall     ID3D12Debug.EnableDebugLayer
            safeRelease [.dbgi]
            ;
            ; DXGI factory
            ;
  @@:       lea         rcx, [IID_IDXGIFactory4]
            lea         rdx, [glob.factory_dxgi]
            icall       CreateDXGIFactory1
            checkhr     eax, .error
            ;
            ; Create device
            ;
            xor         ecx, ecx
            mov         edx, D3D_FEATURE_LEVEL_11_1
            lea         r8, [IID_ID3D12Device]
            lea         r9, [glob.device]
            icall       D3D12CreateDevice
            checkhr     eax, .error
            ;
            ; Create command queue
            ;
            mov         [.cmdqueue_desc.Type], D3D12_COMMAND_LIST_TYPE_DIRECT
            mov         rcx, [glob.device]
            lea         rdx, [.cmdqueue_desc]
            lea         r8, [IID_ID3D12CommandQueue]
            lea         r9, [glob.cmdqueue]
            comcall     ID3D12Device.CreateCommandQueue
            checkhr     eax, .error
            ;
            ; Create swapchain
            ;
            mov         [.swapchain_desc.BufferDesc.Width], eax, [glob.win_width]
            mov         [.swapchain_desc.BufferDesc.Height], eax, [glob.win_height]
            mov         [.swapchain_desc.BufferDesc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
            mov         [.swapchain_desc.SampleDesc.Count], 1
            mov         [.swapchain_desc.BufferCount], k_swapchain_buffers_count
            mov         [.swapchain_desc.OutputWindow], rax, [glob.win_handle]
            mov         [.swapchain_desc.Windowed], 1
            mov         [.swapchain_desc.SwapEffect], DXGI_SWAP_EFFECT_FLIP_DISCARD
            mov         rcx, [glob.factory_dxgi]
            mov         rdx, [glob.cmdqueue]
            lea         r8, [.swapchain_desc]
            lea         r9, [.swapchain]
            comcall     IDXGIFactory4.CreateSwapChain
            checkhr     eax, .error
            mov         rcx, [.swapchain]
            lea         rdx, [IID_IDXGISwapChain3]
            lea         r8, [glob.swapchain]
            comcall     IUnknown.QueryInterface
            safeRelease [.swapchain]
            ;
            ; Setup viewport & scissor
            ;
            vxorps      xmm2, xmm2, xmm2
            vcvtsi2ss   xmm0, xmm2, [glob.win_width]
            vcvtsi2ss   xmm1, xmm2, [glob.win_height]
            vmovd       [glob.viewport.Width], xmm0
            vmovd       [glob.viewport.Height], xmm1
            mov         [glob.viewport.TopLeftX], dword 0.0
            mov         [glob.viewport.TopLeftY], dword 0.0
            mov         [glob.viewport.MinDepth], dword 0.0
            mov         [glob.viewport.MaxDepth], dword 1.0
            mov         [glob.scissor.left], 0
            mov         [glob.scissor.top], 0
            mov         [glob.scissor.right], eax, [glob.win_width]
            mov         [glob.scissor.bottom], eax, [glob.win_height]
            ;
            ; Get descriptor sizes
            ;
            mov         rcx, [glob.device]
            mov         edx, D3D12_DESCRIPTOR_HEAP_TYPE_RTV
            comcall     ID3D12Device.GetDescriptorHandleIncrementSize
            mov         [glob.rtv_size], eax
            mov         rcx, [glob.device]
            mov         edx, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
            comcall     ID3D12Device.GetDescriptorHandleIncrementSize
            mov         [glob.cbv_srv_uav_size], eax
            ;
            ; Create RTV descriptor heap
            ;
            mov         [.rtv_heap_desc.Type], D3D12_DESCRIPTOR_HEAP_TYPE_RTV
            mov         [.rtv_heap_desc.NumDescriptors], k_swapchain_buffers_count
            mov         rcx, [glob.device]
            lea         rdx, [.rtv_heap_desc]
            lea         r8, [IID_ID3D12DescriptorHeap]
            lea         r9, [glob.rtv_heap]
            comcall     ID3D12Device.CreateDescriptorHeap
            checkhr     eax, .error
            mov         rcx, [glob.rtv_heap]
            lea         rdx, [glob.rtv_heap_start]
            comcall     ID3D12DescriptorHeap.GetCPUDescriptorHandleForHeapStart
            mov         [glob.rtv_heap_start], rax, [rax]
            ;
            ; Swapchain buffer descriptors
            ;
            xor         esi, esi
  .for_each_swap_buffer:
            mov         rcx, [glob.swapchain]
            mov         edx, esi
            lea         r8, [IID_ID3D12Resource]
            lea         r9, [glob.swapchain_buffers+rsi*8]
            comcall     IDXGISwapChain3.GetBuffer
            checkhr     eax, .error
            ; render target views
            mov         rcx, [glob.device]
            mov         rdx, [glob.swapchain_buffers+rsi*8]
            xor         r8d, r8d
            mov         r9d, esi
            imul        r9d, [glob.rtv_size]
            add         r9, [glob.rtv_heap_start]
            comcall     ID3D12Device.CreateRenderTargetView
            ; loop counter
            add         esi, 1
            cmp         esi, k_swapchain_buffers_count
            jb          .for_each_swap_buffer
            ;
            ; Init descriptor heap desc
            ;
            mov         [.descriptor_heap_desc.Type], D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
            mov         [.descriptor_heap_desc.NumDescriptors], k_max_descriptors_count
            mov         [.descriptor_heap_desc.Flags], D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE
            ;
            ; Frame resource loop
            ;
            lea         rbx, [glob.frame_res0]     ; rbx = .fres
  .frame_resources_loop:
            ; create command allocator
            mov         rcx, [glob.device]
            mov         edx, D3D12_COMMAND_LIST_TYPE_DIRECT
            lea         r8, [IID_ID3D12CommandAllocator]
            lea         r9, [.fres.cmdalloc]
            comcall     ID3D12Device.CreateCommandAllocator
            checkhr     eax, .error
            ; create constant buffer
            mov         ecx, 1024
            call        create_upload_buffer
            mov         [.fres.constant_buffer], rax
            mov         [.fres.constant_buffer_addr], rdx
            mov         rcx, [.fres.constant_buffer]
            comcall     ID3D12Resource.GetGPUVirtualAddress
            mov         [.fres.constant_buffer_view], rax
            ; create descriptor heap
            mov         rcx, [glob.device]
            lea         rdx, [.descriptor_heap_desc]
            lea         r8, [IID_ID3D12DescriptorHeap]
            lea         r9, [.fres.dheap]
            comcall     ID3D12Device.CreateDescriptorHeap
            checkhr     eax, .error
            ; get CPU handle
            mov         rcx, [.fres.dheap]
            lea         rdx, [.fres.dheap_cpu_start]
            comcall     ID3D12DescriptorHeap.GetCPUDescriptorHandleForHeapStart
            mov         [.fres.dheap_cpu_start], rax, [rax]
            ; get GPU handle
            mov         rcx, [.fres.dheap]
            lea         rdx, [.fres.dheap_gpu_start]
            comcall     ID3D12DescriptorHeap.GetGPUDescriptorHandleForHeapStart
            mov         [.fres.dheap_gpu_start], rax, [rax]
            ; loop counter
            add         rbx, sizeof.frame_resources
            cmp         rbx, glob.frame_res0 + k_buffered_frames_count * sizeof.frame_resources
            jb          .frame_resources_loop
            ;
            ; Create cpu only descriptor heap
            ;
            mov         [.descriptor_heap_desc.NumDescriptors], k_max_descriptors_count
            mov         [.descriptor_heap_desc.Flags], 0
            mov         rcx, [glob.device]
            lea         rdx, [.descriptor_heap_desc]
            lea         r8, [IID_ID3D12DescriptorHeap]
            lea         r9, [glob.cbv_srv_uav_heap]
            comcall     ID3D12Device.CreateDescriptorHeap
            checkhr     eax, .error
            ; get CPU handle
            mov         rcx, [glob.cbv_srv_uav_heap]
            lea         rdx, [glob.cbv_srv_uav_heap_start]
            comcall     ID3D12DescriptorHeap.GetCPUDescriptorHandleForHeapStart
            mov         [glob.cbv_srv_uav_heap_start], rax, [rax]
            ;
            ; Create frame fence
            ;
            mov         rcx, [glob.device]
            mov         edx, 0
            mov         r8d, D3D12_FENCE_FLAG_NONE
            lea         r9, [IID_ID3D12Fence]
            lea         [.param5], rax, [glob.frame_fence]
            comcall     ID3D12Device.CreateFence
            checkhr     eax, .error
            ;
            ; Create frame fence event
            ;
            xor         ecx, ecx
            xor         edx, edx
            xor         r8d, r8d
            mov         r9d, EVENT_ALL_ACCESS
            icall       CreateEventEx
            mov         [glob.frame_fence_event], rax
            test        rax, rax
            jz          .error
            ;
            ; Create command list
            ;
            lea         rbx, [glob.frame_res0]
            mov         rcx, [glob.device]
            mov         edx, 0
            mov         r8d, D3D12_COMMAND_LIST_TYPE_DIRECT
            mov         r9, [.fres.cmdalloc]
            mov         [.param5], 0
            lea         [.param6], rax, [IID_ID3D12GraphicsCommandList]
            lea         [.param7], rax, [glob.cmdlist]
            comcall     ID3D12Device.CreateCommandList
            checkhr     eax, .error
            ;
            ; Init scene
            ;
            sceneInit
            test        eax, eax
            jz          .error
            ;
            ; Close & execute command list
            ;
            mov         rcx, [glob.cmdlist]
            comcall     ID3D12GraphicsCommandList.Close
            checkhr     eax, .error
            mov         rcx, [glob.cmdqueue]
            mov         edx, 1
            lea         r8, [glob.cmdlist]
            comcall     ID3D12CommandQueue.ExecuteCommandLists
            call        wait_for_gpu
            ;
            ; Return success
            ;
            mov         eax, 1
            jmp         .return
  .error:   xor         eax, eax
  .return:  add         rsp, .k_stack_size
            pop         rbx rsi
            ret
;=============================================================================
falign
demo_deinit:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+16
            push        rbx
            sub         rsp, .k_stack_size

            sceneDeinit

            lea         rbx, [glob.frame_res0]
  virtual at rbx
  .fres frame_resources
  end virtual

  .frame_resources_loop:
            safeRelease [.fres.constant_buffer]
            safeRelease [.fres.cmdalloc]
            safeRelease [.fres.dheap]
            add         rbx, sizeof.frame_resources
            cmp         rbx, glob.frame_res0 + k_buffered_frames_count * sizeof.frame_resources
            jb          .frame_resources_loop

            xor         ebx, ebx
  .upload_buffers_loop:
            safeRelease [glob.upload_buffers+rbx*8]
            add         ebx, 1
            cmp         ebx, [glob.upload_buffers_count]
            jne         .upload_buffers_loop

            xor         ebx, ebx
  .swapchain_buffers_loop:
            safeRelease [glob.swapchain_buffers+rbx*8]
            add         ebx, 1
            cmp         ebx, k_swapchain_buffers_count
            jne         .swapchain_buffers_loop

            safeRelease [glob.cmdlist]
            safeRelease [glob.frame_fence]
            safeRelease [glob.rtv_heap]
            safeRelease [glob.cbv_srv_uav_heap]
            safeRelease [glob.swapchain]
            safeRelease [glob.cmdqueue]
            safeRelease [glob.device]
            safeRelease [glob.factory_dxgi]

  @@:       add         rsp, .k_stack_size
            pop         rbx
            ret
;=============================================================================
falign
demo_update:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+16
            push        rsi
            sub         rsp, .k_stack_size

            sceneUpdate
            ;test eax, eax
            ;jz .error

            ; present
            mov         rcx, [glob.swapchain]
            xor         edx, edx
            xor         r8d, r8d
            comcall     IDXGISwapChain3.Present

            add         [glob.cpu_completed_fences], 1

            mov         rcx, [glob.cmdqueue]
            mov         rdx, [glob.frame_fence]
            mov         r8, [glob.cpu_completed_fences]
            comcall     ID3D12CommandQueue.Signal

            mov         rcx, [glob.frame_fence]
            comcall     ID3D12Fence.GetCompletedValue
            mov         rsi, rax
            mov         rax, [glob.cpu_completed_fences]
            sub         rax, rsi
            cmp         eax, k_buffered_frames_count
            jb          @f

            ; wait for the GPU to finish one frame
            mov         rcx, [glob.frame_fence]
            mov         rdx, rsi
            add         rdx, 1
            mov         r8, [glob.frame_fence_event]
            comcall     ID3D12Fence.SetEventOnCompletion

            mov         rcx, [glob.frame_fence_event]
            mov         edx, INFINITE
            icall       WaitForSingleObject

            ; update back_buffer_index and frame_index
  @@:       mov         rcx, [glob.swapchain]
            comcall     IDXGISwapChain3.GetCurrentBackBufferIndex
            mov         [glob.back_buffer_index], eax

            xor         edx, edx
            mov         eax, [glob.frame_index]
            add         eax, 1
            mov         ecx, k_buffered_frames_count
            div         rcx
            mov         [glob.frame_index], edx

            mov         eax, 1
            jmp         .return
  .error:   xor         eax, eax
  .return:  add         rsp, .k_stack_size
            pop         rsi
            ret
;=============================================================================
