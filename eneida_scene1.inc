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
        $lea rbx, [glob.frame_res]
  virtual at rbx
  .frame_res frame_resources
  end virtual

  .frame_resources_loop:
        $mov ecx, 1024
        $call create_upload_buffer
        $mov [.frame_res.constant_buffer], rax
        $mov [.frame_res.constant_buffer_addr], rdx

        ; create constant buffer view
        $mov rcx, [.frame_res.constant_buffer]
        $comcall ID3D12Resource.GetGPUVirtualAddress
        $mov [.frame_res.constant_buffer_view], rax

        $add rbx, sizeof.frame_resources
        $sub esi, 1
        $jnz .frame_resources_loop



        ; create vertex buffer
        $mov ecx, 1024
        $call create_upload_buffer
        $mov [glob.mesh_vb], rax

        ; fill vertex buffer
        $vmovaps xmm0, [glob.tri_v0]
        $vmovaps [rdx], xmm0
        $vmovaps xmm0, [glob.tri_v1]
        $vmovaps [rdx+16], xmm0
        $vmovaps xmm0, [glob.tri_v2]
        $vmovaps [rdx+32], xmm0

        ; create vertex buffer view
        $mov rcx, [glob.mesh_vb]
        $comcall ID3D12Resource.GetGPUVirtualAddress
        $mov [glob.mesh_vb_view.BufferLocation], rax
        $mov [glob.mesh_vb_view.SizeInBytes], 1024
        $mov [glob.mesh_vb_view.StrideInBytes], 16


        ; load VS
        $lea rcx, [glob.sz_vs_object]
        $call load_file
        $mov [.pso_desc.VS.pShaderBytecode], rax
        $mov [.pso_desc.VS.BytecodeLength], rdx
        ; load PS
        $lea rcx, [glob.sz_ps_object]
        $call load_file
        $mov [.pso_desc.PS.pShaderBytecode], rax
        $mov [.pso_desc.PS.BytecodeLength], rdx

        $lea [.input_element_desc.SemanticName], rax, [glob.sz_position]
        $mov [.input_element_desc.SemanticIndex], 0
        $mov [.input_element_desc.Format], DXGI_FORMAT_R32G32B32A32_FLOAT
        $mov [.input_element_desc.InputSlot], 0
        $mov [.input_element_desc.AlignedByteOffset], 0

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

        $mov rcx, [glob.device]
        $lea rdx, [.pso_desc]
        $lea r8, [IID_ID3D12PipelineState]
        $lea r9, [glob.pso]
        $comcall ID3D12Device.CreateGraphicsPipelineState
        $checkhr eax, .error

        $mov rcx, [glob.device]
        $xor edx, edx
        $mov r8, [.pso_desc.VS.pShaderBytecode]
        $mov r9, [.pso_desc.VS.BytecodeLength]
        $lea [.param5], rax, [IID_ID3D12RootSignature]
        $lea [.param6], rax, [glob.rootsig]
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

        $mov r12d, [glob.frame_index]
        $imul r12d, sizeof.frame_resources
        $lea r12, [r12+glob.frame_res]
  virtual at r12
  .frame_res frame_resources
  end virtual

        ; init transition barrier
        $mov [.barrier.Type], D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
        $mov [.barrier.Flags], 0
        $mov [.barrier.Transition.Subresource], D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES


        ; update rotation matrix
        $vmovss xmm0, [glob.eye_half_fovy]
        $call fast_sincosf
        $vmovss xmm2, [glob.viewport.Width]
        $vdivss xmm2, xmm2, [glob.viewport.Height]
        $vmovss xmm3, [glob.eye_nearz]
        $vmovss xmm4, [glob.eye_farz]
        $matrixPerspectiveFov
        $vmovaps [.matrix], ymm0
        $vmovaps [.matrix+32], ymm1

        $vmovaps xmm0, [glob.eye_position]
        $vmovaps xmm1, [glob.eye_focus]
        $vmovaps xmm2, [glob.eye_up]
        $call matrix_look_at

        $vmovaps ymm2, [.matrix]
        $vmovaps ymm3, [.matrix+32]
        $call matrix_mul                        ; lookat*perspective
        $vmovaps [.matrix], ymm0
        $vmovaps [.matrix+32], ymm1

        $vcvtsd2ss xmm0, xmm0, [glob.time]
        $call fast_sincosf
        $matrixRotationX
        $vmovaps ymm2, [.matrix]
        $vmovaps ymm3, [.matrix+32]
        $call matrix_mul_transpose              ; transpose(rotation*lookat*perspective)


        ; update constant buffer
        $mov rcx, [.frame_res.constant_buffer_addr]
        $vmovaps [rcx], ymm0
        $vmovaps [rcx+32], ymm1



        ; reset completed command allocator
        $mov rcx, [.frame_res.cmdalloc]
        $comcall ID3D12CommandAllocator.Reset

        ; begin commands recording
        $mov rcx, [glob.cmdlist]
        $mov rdx, [.frame_res.cmdalloc]
        $mov r8, [glob.pso]
        $comcall ID3D12GraphicsCommandList.Reset

        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $lea r8, [glob.viewport]
        $comcall ID3D12GraphicsCommandList.RSSetViewports

        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $lea r8, [glob.scissor]
        $comcall ID3D12GraphicsCommandList.RSSetScissorRects

        ; set root signature and resources
        $mov rcx, [glob.cmdlist]
        $mov rdx, [glob.rootsig]
        $comcall ID3D12GraphicsCommandList.SetGraphicsRootSignature

        $mov rcx, [glob.cmdlist]
        $xor edx, edx
        $mov r8, [.frame_res.constant_buffer_view]
        $comcall ID3D12GraphicsCommandList.SetGraphicsRootConstantBufferView

        ; vertex buffer view
        $mov rcx, [glob.cmdlist]
        $mov edx, 0
        $mov r8d, 1
        $lea r9, [glob.mesh_vb_view]
        $comcall ID3D12GraphicsCommandList.IASetVertexBuffers

        ; back buffer to D3D12_RESOURCE_STATE_RENDER_TARGET
        $mov eax, [glob.back_buffer_index]
        $mov rax, [glob.swapchain_buffers+rax*8]
        $transitionBarrier .barrier, rax, D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET
        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $lea r8, [.barrier]
        $comcall ID3D12GraphicsCommandList.ResourceBarrier

        ; get back buffer handle
        $mov eax, [glob.back_buffer_index]
        $imul eax, [glob.rtv_size]
        $add rax, [glob.swapchain_heap_start]
        $mov [.back_buffer_handle], rax

        ; clear back buffer
        $mov rcx, [glob.cmdlist]
        $mov rdx, [.back_buffer_handle]
        $lea r8, [glob.clear_color]
        $xor r9d, r9d
        $mov [.param5], 0
        $comcall ID3D12GraphicsCommandList.ClearRenderTargetView

        ; set back buffer as render target
        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $lea r8, [.back_buffer_handle]
        $mov r9d, 1
        $mov [.param5], 0
        $comcall ID3D12GraphicsCommandList.OMSetRenderTargets

        ; draw
        $mov rcx, [glob.cmdlist]
        $mov edx, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST
        $comcall ID3D12GraphicsCommandList.IASetPrimitiveTopology
        $mov rcx, [glob.cmdlist]
        $mov edx, 3
        $mov r8d, 1
        $xor r9d, r9d
        $mov [.param5], 0
        $comcall ID3D12GraphicsCommandList.DrawInstanced

        ; back buffer to D3D12_RESOURCE_STATE_PRESENT
        $mov eax, [glob.back_buffer_index]
        $mov rax, [glob.swapchain_buffers+rax*8]
        $transitionBarrier .barrier, rax, D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT
        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $lea r8, [.barrier]
        $comcall ID3D12GraphicsCommandList.ResourceBarrier


        ; finish recording and execute commands
        $mov rcx, [glob.cmdlist]
        $comcall ID3D12GraphicsCommandList.Close
        $mov rcx, [glob.cmdqueue]
        $mov edx, 1
        $lea r8, [glob.cmdlist]
        $comcall ID3D12CommandQueue.ExecuteCommandLists

        $add rsp, .k_stack_size
        $pop r12 rdi rsi
        $ret
;=============================================================================
