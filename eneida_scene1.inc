;=============================================================================
falign
scene1_init:
;-----------------------------------------------------------------------------
        $push rsi rbx
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 8 n { .param#n dq ? }
  dalign 8
  .pso_desc D3D12_GRAPHICS_PIPELINE_STATE_DESC
  dalign 8
  .upload_heap_props D3D12_HEAP_PROPERTIES
  dalign 8
  .buffer_res_desc D3D12_RESOURCE_DESC
  dalign 8
  .empty_range D3D12_RANGE
  dalign 8
  .input_element_desc D3D12_INPUT_ELEMENT_DESC
  dalign 32
  .k_stack_size = $-$$+8
  end virtual
        $zeroStack .k_stack_size

        $mov [.upload_heap_props.Type], D3D12_HEAP_TYPE_UPLOAD
        $mov [.buffer_res_desc.Dimension], D3D12_RESOURCE_DIMENSION_BUFFER
        $mov [.buffer_res_desc.Width], 1024
        $mov [.buffer_res_desc.Height], 1
        $mov [.buffer_res_desc.DepthOrArraySize], 1
        $mov [.buffer_res_desc.MipLevels], 1
        $mov [.buffer_res_desc.SampleDesc.Count], 1
        $mov [.buffer_res_desc.Layout], D3D12_TEXTURE_LAYOUT_ROW_MAJOR

        $mov esi, k_frame_count
        $lea rbx, [scene1.frame_resources0]
  virtual at rbx
  .frame_res frame_resources
  end virtual

  .frame_resources_loop:
        ; constant buffer: create
        $mov rcx, [demo.device]
        $lea rdx, [.upload_heap_props]
        $mov r8d, D3D12_HEAP_FLAG_NONE
        $lea r9, [.buffer_res_desc]
        $mov [.param5], D3D12_RESOURCE_STATE_GENERIC_READ
        $mov [.param6], 0
        $lea [.param7], rax, [IID_ID3D12Resource]
        $lea [.param8], rax, [.frame_res.constant_buffer]
        $comcall ID3D12Device.CreateCommittedResource
        $checkhr eax, .error

        ; vertex buffer: create
        $mov rcx, [demo.device]
        $lea rdx, [.upload_heap_props]
        $mov r8d, D3D12_HEAP_FLAG_NONE
        $lea r9, [.buffer_res_desc]
        $mov [.param5], D3D12_RESOURCE_STATE_GENERIC_READ
        $mov [.param6], 0
        $lea [.param7], rax, [IID_ID3D12Resource]
        $lea [.param8], rax, [.frame_res.vertex_buffer]
        $comcall ID3D12Device.CreateCommittedResource
        $checkhr eax, .error

        ; constant buffer: map (get CPU address)
        $mov rcx, [.frame_res.constant_buffer]
        $xor edx, edx
        $lea r8, [.empty_range]
        $lea r9, [.frame_res.constant_buffer_ptr]
        $comcall ID3D12Resource.Map
        $checkhr eax, .error

        ; vertex buffer: map (get CPU address)
        $mov rcx, [.frame_res.vertex_buffer]
        $xor edx, edx
        $lea r8, [.empty_range]
        $lea r9, [.frame_res.vertex_buffer_ptr]
        $comcall ID3D12Resource.Map
        $checkhr eax, .error

        ; constant buffer: get GPU address
        $mov rcx, [.frame_res.constant_buffer]
        $comcall ID3D12Resource.GetGPUVirtualAddress
        $mov [.frame_res.constant_buffer_gpu_ptr], rax

        ; vertex buffer: get GPU address
        $mov rcx, [.frame_res.vertex_buffer]
        $comcall ID3D12Resource.GetGPUVirtualAddress
        $mov [.frame_res.vertex_buffer_view.BufferLocation], rax
        $mov [.frame_res.vertex_buffer_view.SizeInBytes], 1024
        $mov [.frame_res.vertex_buffer_view.StrideInBytes], 16

        $add rbx, sizeof.frame_resources
        $sub esi, 1
        $jnz .frame_resources_loop


        $lea [.input_element_desc.SemanticName], rax, [scene1.s_position]
        $mov [.input_element_desc.SemanticIndex], 0
        $mov [.input_element_desc.Format], DXGI_FORMAT_R32G32B32A32_FLOAT
        $mov [.input_element_desc.InputSlot], 0
        $mov [.input_element_desc.AlignedByteOffset], 0

        $lea [.pso_desc.VS.pShaderBytecode], rax, [cso_object_vs]
        $mov [.pso_desc.VS.BytecodeLength], cso_object_vs.size
        $lea [.pso_desc.PS.pShaderBytecode], rax, [cso_object_ps]
        $mov [.pso_desc.PS.BytecodeLength], cso_object_ps.size
        $lea [.pso_desc.InputLayout.pInputElementDescs], rax, [.input_element_desc]
        $mov [.pso_desc.InputLayout.NumElements], 1
        $mov [.pso_desc.BlendState.RenderTarget0.RenderTargetWriteMask], $0f
        $mov [.pso_desc.SampleMask], $ffffffff
        $mov [.pso_desc.RasterizerState.CullMode], D3D12_CULL_MODE_NONE
        $mov [.pso_desc.RasterizerState.FillMode], D3D12_FILL_MODE_SOLID
        $mov [.pso_desc.PrimitiveTopologyType], D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE
        $mov [.pso_desc.NumRenderTargets], 1
        $mov [.pso_desc.RTVFormats], DXGI_FORMAT_R8G8B8A8_UNORM
        $mov [.pso_desc.SampleDesc.Count], 1

        $mov rcx, [demo.device]
        $lea rdx, [.pso_desc]
        $lea r8, [IID_ID3D12PipelineState]
        $lea r9, [scene1.pso]
        $comcall ID3D12Device.CreateGraphicsPipelineState
        $checkhr eax, .error

        $mov rcx, [demo.device]
        $xor edx, edx
        $lea r8, [cso_object_vs]
        $mov r9d, cso_object_vs.size
        $lea [.param5], rax, [IID_ID3D12RootSignature]
        $lea [.param6], rax, [scene1.rootsig]
        $comcall ID3D12Device.CreateRootSignature
        $checkhr eax, .error

        ; success
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
scene1_deinit:
;-----------------------------------------------------------------------------
        $ret
;=============================================================================
falign
scene1_update:
;-----------------------------------------------------------------------------
        $push rsi rdi r12
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 5 n { .param#n dq ? }
  .barrier D3D12_RESOURCE_BARRIER
  dalign 8
  .back_buffer_handle dq ?
  dalign 32
  .matrix: rd 16
  dalign 32
  .k_stack_size = $-$$
  end virtual

        $mov r12d, [demo.frame_index]
        $imul r12d, sizeof.frame_resources
        $lea r12, [r12+scene1.frame_resources0]
  virtual at r12
  .frame_res frame_resources
  end virtual

        ; update rotation matrix
        $vmovss xmm0, [scene1.eye_half_fovy]
        $call fast_sincosf
        $vmovss xmm2, [demo.viewport.Width]
        $vdivss xmm2, xmm2, [demo.viewport.Height]
        $vmovss xmm3, [scene1.eye_nearz]
        $vmovss xmm4, [scene1.eye_farz]
        $matrixPerspectiveFov
        $vmovaps [.matrix], ymm0
        $vmovaps [.matrix+32], ymm1

        $vmovaps xmm0, [scene1.eye_position]
        $vmovaps xmm1, [scene1.eye_focus]
        $vmovaps xmm2, [scene1.eye_up]
        $call matrix_look_at

        $vmovaps ymm2, [.matrix]
        $vmovaps ymm3, [.matrix+32]
        $call matrix_mul                        ; lookat*perspective
        $vmovaps [.matrix], ymm0
        $vmovaps [.matrix+32], ymm1

        $vcvtsd2ss xmm0, xmm0, [time]
        $call fast_sincosf
        $matrixRotationX
        $vmovaps ymm2, [.matrix]
        $vmovaps ymm3, [.matrix+32]
        $call matrix_mul_transpose              ; transpose(rotation*lookat*perspective)

        ; update constant buffer
        $mov rcx, [.frame_res.constant_buffer_ptr]
        $vmovaps [rcx], ymm0
        $vmovaps [rcx+32], ymm1

        ; update vertex buffer
        $mov rcx, [.frame_res.vertex_buffer_ptr]
        $vmovaps xmm0, [scene1.tri_v0]
        $vmovaps [rcx], xmm0
        $vmovaps xmm0, [scene1.tri_v1]
        $vmovaps [rcx+16], xmm0
        $vmovaps xmm0, [scene1.tri_v2]
        $vmovaps [rcx+32], xmm0

        ; init transition barrier
        $mov [.barrier.Type], D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
        $mov [.barrier.Flags], 0
        $mov [.barrier.Transition.Subresource], D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES

        ; reset completed command allocator
        $mov eax, [demo.frame_index]
        $mov rcx, [demo.cmdalloc+rax*8]
        $comcall ID3D12CommandAllocator.Reset

        ; begin commands recording
        $mov eax, [demo.frame_index]
        $mov rcx, [demo.cmdlist]
        $mov rdx, [demo.cmdalloc+rax*8]
        $mov r8, [scene1.pso]
        $comcall ID3D12GraphicsCommandList.Reset

        $mov rcx, [demo.cmdlist]
        $mov edx, 1
        $lea r8, [demo.viewport]
        $comcall ID3D12GraphicsCommandList.RSSetViewports
        $mov rcx, [demo.cmdlist]
        $mov edx, 1
        $lea r8, [demo.scissor]
        $comcall ID3D12GraphicsCommandList.RSSetScissorRects

        ; set root signature and resources
        $mov rcx, [demo.cmdlist]
        $mov rdx, [scene1.rootsig]
        $comcall ID3D12GraphicsCommandList.SetGraphicsRootSignature

        $mov rcx, [demo.cmdlist]
        $xor edx, edx
        $mov r8, [.frame_res.constant_buffer_gpu_ptr]
        $comcall ID3D12GraphicsCommandList.SetGraphicsRootConstantBufferView

        ; vertex buffer view
        $mov rcx, [demo.cmdlist]
        $xor edx, edx
        $mov r8d, 1
        $lea r9, [.frame_res.vertex_buffer_view]
        $comcall ID3D12GraphicsCommandList.IASetVertexBuffers

        ; back buffer to D3D12_RESOURCE_STATE_RENDER_TARGET
        $mov eax, [demo.back_buffer_index]
        $mov rax, [demo.swapchain_buffer+rax*8]
        $transitionBarrier .barrier, rax, D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET
        $mov rcx, [demo.cmdlist]
        $mov edx, 1
        $lea r8, [.barrier]
        $comcall ID3D12GraphicsCommandList.ResourceBarrier

        ; get back buffer handle
        $mov eax, [demo.back_buffer_index]
        $imul eax, [demo.rtv_size]
        $add rax, [demo.swapchain_heap_start]
        $mov [.back_buffer_handle], rax

        ; clear back buffer
        $mov rcx, [demo.cmdlist]
        $mov rdx, [.back_buffer_handle]
        $lea r8, [demo.clear_color]
        $xor r9d, r9d
        $mov [.param5], 0
        $comcall ID3D12GraphicsCommandList.ClearRenderTargetView

        ; set back buffer as render target
        $mov rcx, [demo.cmdlist]
        $mov edx, 1
        $lea r8, [.back_buffer_handle]
        $mov r9d, 1
        $mov [.param5], 0
        $comcall ID3D12GraphicsCommandList.OMSetRenderTargets

        ; draw
        $mov rcx, [demo.cmdlist]
        $mov edx, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST
        $comcall ID3D12GraphicsCommandList.IASetPrimitiveTopology
        $mov rcx, [demo.cmdlist]
        $mov edx, 3
        $mov r8d, 1
        $xor r9d, r9d
        $mov [.param5], 0
        $comcall ID3D12GraphicsCommandList.DrawInstanced

        ; back buffer to D3D12_RESOURCE_STATE_PRESENT
        $mov eax, [demo.back_buffer_index]
        $mov rax, [demo.swapchain_buffer+rax*8]
        $transitionBarrier .barrier, rax, D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT
        $mov rcx, [demo.cmdlist]
        $mov edx, 1
        $lea r8, [.barrier]
        $comcall ID3D12GraphicsCommandList.ResourceBarrier

        ; finish recording and execute commands
        $mov rcx, [demo.cmdlist]
        $comcall ID3D12GraphicsCommandList.Close
        $mov rcx, [demo.cmdqueue]
        $mov edx, 1
        $lea r8, [demo.cmdlist]
        $comcall ID3D12CommandQueue.ExecuteCommandLists

        $add rsp, .k_stack_size
        $pop r12 rdi rsi
        $ret
;=============================================================================
