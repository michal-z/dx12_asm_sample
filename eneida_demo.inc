;=============================================================================
falign
demo_init:
;-----------------------------------------------------------------------------
        $push rsi rbx
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 7 n { .param#n dq ? }
  .dbgi dq ?
  .swapchain dq ?
  dalign 8
  .cmdqueue_desc D3D12_COMMAND_QUEUE_DESC
  dalign 8
  .swapchain_desc DXGI_SWAP_CHAIN_DESC
  dalign 8
  .swapchain_heap_desc D3D12_DESCRIPTOR_HEAP_DESC
  dalign 32
  .k_stack_size = $-$$+8
  end virtual
        $zeroStack .k_stack_size

        ; debug layer
        $lea rcx, [IID_ID3D12Debug]
        $lea rdx, [.dbgi]
        $icall D3D12GetDebugInterface
        $checkhr eax, @f
        $mov rcx, [.dbgi]
        $comcall ID3D12Debug.EnableDebugLayer
        $safeRelease [.dbgi]

        ; DXGI factory
  @@:   $lea rcx, [IID_IDXGIFactory4]
        $lea rdx, [glob.factory_dxgi]
        $icall CreateDXGIFactory1
        $checkhr eax, .error

        ; device
        $xor ecx, ecx
        $mov edx, D3D_FEATURE_LEVEL_11_1
        $lea r8, [IID_ID3D12Device]
        $lea r9, [glob.device]
        $icall D3D12CreateDevice
        $checkhr eax, .error

        ; command queue
        $mov [.cmdqueue_desc.Type], D3D12_COMMAND_LIST_TYPE_DIRECT
        $mov rcx, [glob.device]
        $lea rdx, [.cmdqueue_desc]
        $lea r8, [IID_ID3D12CommandQueue]
        $lea r9, [glob.cmdqueue]
        $comcall ID3D12Device.CreateCommandQueue
        $checkhr eax, .error

        ; command allocators
        $lea rbx, [glob.frame_res]
  virtual at rbx
  .fres frame_resources
  end virtual

        $xor esi, esi
  @@:   $mov rcx, [glob.device]
        $mov edx, D3D12_COMMAND_LIST_TYPE_DIRECT
        $lea r8, [IID_ID3D12CommandAllocator]
        $lea r9, [.fres.cmdalloc]
        $comcall ID3D12Device.CreateCommandAllocator
        $checkhr eax, .error

        $add rbx, sizeof.frame_resources
        $add esi, 1
        $cmp esi, k_frame_count
        $jb @b

        ; swapchain
        $mov [.swapchain_desc.BufferDesc.Width], eax, [glob.win_width]
        $mov [.swapchain_desc.BufferDesc.Height], eax, [glob.win_height]
        $mov [.swapchain_desc.BufferDesc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
        $mov [.swapchain_desc.SampleDesc.Count], 1
        $mov [.swapchain_desc.BufferCount], k_swapchain_buffer_count
        $mov [.swapchain_desc.OutputWindow], rax, [glob.win_handle]
        $mov [.swapchain_desc.Windowed], 1
        $mov [.swapchain_desc.SwapEffect], DXGI_SWAP_EFFECT_FLIP_DISCARD

        $mov rcx, [glob.factory_dxgi]
        $mov rdx, [glob.cmdqueue]
        $lea r8, [.swapchain_desc]
        $lea r9, [.swapchain]
        $comcall IDXGIFactory4.CreateSwapChain
        $checkhr eax, .error

        $mov rcx, [.swapchain]
        $lea rdx, [IID_IDXGISwapChain3]
        $lea r8, [glob.swapchain]
        $comcall IUnknown.QueryInterface
        $safeRelease [.swapchain]

        ; viewport & scissor
        $vxorps xmm2, xmm2, xmm2
        $vcvtsi2ss xmm0, xmm2, [glob.win_width]
        $vcvtsi2ss xmm1, xmm2, [glob.win_height]
        $vmovd [glob.viewport.Width], xmm0
        $vmovd [glob.viewport.Height], xmm1
        $mov [glob.viewport.TopLeftX], dword 0.0
        $mov [glob.viewport.TopLeftY], dword 0.0
        $mov [glob.viewport.MinDepth], dword 0.0
        $mov [glob.viewport.MaxDepth], dword 1.0
        $mov [glob.scissor.left], 0
        $mov [glob.scissor.top], 0
        $mov [glob.scissor.right], eax, [glob.win_width]
        $mov [glob.scissor.bottom], eax, [glob.win_height]

        ; get descriptor sizes
        $mov rcx, [glob.device]
        $mov edx, D3D12_DESCRIPTOR_HEAP_TYPE_RTV
        $comcall ID3D12Device.GetDescriptorHandleIncrementSize
        $mov [glob.rtv_size], eax

        $mov rcx, [glob.device]
        $mov edx, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
        $comcall ID3D12Device.GetDescriptorHandleIncrementSize
        $mov [glob.cbv_srv_uav_size], eax

        ; swapchain descriptor heap
        $mov [.swapchain_heap_desc.Type], D3D12_DESCRIPTOR_HEAP_TYPE_RTV
        $mov [.swapchain_heap_desc.NumDescriptors], k_swapchain_buffer_count
        $mov rcx, [glob.device]
        $lea rdx, [.swapchain_heap_desc]
        $lea r8, [IID_ID3D12DescriptorHeap]
        $lea r9, [glob.swapchain_heap]
        $comcall ID3D12Device.CreateDescriptorHeap
        $checkhr eax, .error

        $mov rcx, [glob.swapchain_heap]
        $lea rdx, [glob.swapchain_heap_start]
        $comcall ID3D12DescriptorHeap.GetCPUDescriptorHandleForHeapStart
        $mov [glob.swapchain_heap_start], rax, [rax]

        ; swapchain buffer descriptors
        $xor esi, esi
  .for_each_swap_buffer:
        $mov rcx, [glob.swapchain]
        $mov edx, esi
        $lea r8, [IID_ID3D12Resource]
        $lea r9, [glob.swapchain_buffers+rsi*8]
        $comcall IDXGISwapChain3.GetBuffer
        $checkhr eax, .error

        $mov rcx, [glob.device]
        $mov rdx, [glob.swapchain_buffers+rsi*8]
        $xor r8d, r8d
        $mov r9d, esi
        $imul r9d, [glob.rtv_size]
        $add r9, [glob.swapchain_heap_start]
        $comcall ID3D12Device.CreateRenderTargetView

        $add esi, 1
        $cmp esi, k_swapchain_buffer_count
        $jb .for_each_swap_buffer


        ; frame fence
        $mov rcx, [glob.device]
        $mov edx, 0
        $mov r8d, D3D12_FENCE_FLAG_NONE
        $lea r9, [IID_ID3D12Fence]
        $lea [.param5], rax, [glob.frame_fence]
        $comcall ID3D12Device.CreateFence
        $checkhr eax, .error

        ; frame fence event
        $xor ecx, ecx
        $xor edx, edx
        $xor r8d, r8d
        $mov r9d, EVENT_ALL_ACCESS
        $icall CreateEventEx
        $mov [glob.frame_fence_event], rax
        $test rax, rax
        $jz .error

        ; create command list
        $lea rbx, [glob.frame_res]
        $mov rcx, [glob.device]
        $xor edx, edx
        $mov r8d, D3D12_COMMAND_LIST_TYPE_DIRECT
        $mov r9, [.fres.cmdalloc]
        $mov [.param5], 0
        $lea [.param6], rax, [IID_ID3D12GraphicsCommandList]
        $lea [.param7], rax, [glob.cmdlist]
        $comcall ID3D12Device.CreateCommandList
        $checkhr eax, .error

        ; init scene
        $call scene1_init
        $test eax, eax
        $jz .error

        ; close & execute command list
        $mov rcx, [glob.cmdlist]
        $comcall ID3D12GraphicsCommandList.Close
        $checkhr eax, .error

        $mov rcx, [glob.cmdqueue]
        $mov edx, 1
        $lea r8, [glob.cmdlist]
        $comcall ID3D12CommandQueue.ExecuteCommandLists

        $call wait_for_gpu

        $mov eax, 1
        $jmp .return
  .error:
        $xor eax, eax
  .return:
        $add rsp, .k_stack_size
        $pop rbx rsi
        $ret
;=============================================================================
falign
demo_deinit:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
        $sub rsp, .k_stack_size

        $safeRelease [glob.cmdlist]
        $safeRelease [glob.frame_fence]
        $safeRelease [glob.swapchain_heap]
        $safeRelease [glob.swapchain]

  @@:   $add rsp, .k_stack_size
        $ret
;=============================================================================
falign
demo_update:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+16
        $push rsi
        $sub rsp, .k_stack_size
        $call scene1_update
        ;$test eax, eax
        ;$jz .error

        ; present
        $mov rcx, [glob.swapchain]
        $xor edx, edx
        $xor r8d, r8d
        $comcall IDXGISwapChain3.Present

        $add [glob.cpu_completed_fences], 1

        $mov rcx, [glob.cmdqueue]
        $mov rdx, [glob.frame_fence]
        $mov r8, [glob.cpu_completed_fences]
        $comcall ID3D12CommandQueue.Signal

        $mov rcx, [glob.frame_fence]
        $comcall ID3D12Fence.GetCompletedValue
        $mov rsi, rax
        $mov rax, [glob.cpu_completed_fences]
        $sub rax, rsi
        $cmp eax, k_frame_count
        $jb @f

        ; wait for the GPU to finish one frame
        $mov rcx, [glob.frame_fence]
        $mov rdx, rsi
        $add rdx, 1
        $mov r8, [glob.frame_fence_event]
        $comcall ID3D12Fence.SetEventOnCompletion

        $mov rcx, [glob.frame_fence_event]
        $mov edx, INFINITE
        $icall WaitForSingleObject

        ; update back_buffer_index and frame_index
  @@:   $mov rcx, [glob.swapchain]
        $comcall IDXGISwapChain3.GetCurrentBackBufferIndex
        $mov [glob.back_buffer_index], eax

        $xor edx, edx
        $mov eax, [glob.frame_index]
        $add eax, 1
        $mov ecx, k_frame_count
        $div rcx
        $mov [glob.frame_index], edx

        $mov eax, 1
        $jmp .return
  .error:
        $xor eax, eax
  .return:
        $add rsp, .k_stack_size
        $pop rsi
        $ret
;=============================================================================
