;=============================================================================
falign
scene1_init:
;-----------------------------------------------------------------------------
        $push rsi rbx
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 6 n { .param#n dq ? }
  dalign 8
  .pso_desc D3D12_GRAPHICS_PIPELINE_STATE_DESC
  dalign 8
  rept 2 n:0 { .input_element_desc#n D3D12_INPUT_ELEMENT_DESC }
  dalign 8
  rept 2 n:0 { .barrier#n D3D12_RESOURCE_BARRIER }
  dalign 8
  .srv_desc D3D12_SHADER_RESOURCE_VIEW_DESC
  dalign 32
  .k_stack_size = $-$$+8
  end virtual
        $zeroStack .k_stack_size

        ; init transition barriers
        $mov [.barrier0.Type], D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
        $mov [.barrier0.Transition.Subresource], D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES
        $mov [.barrier1.Type], D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
        $mov [.barrier1.Transition.Subresource], D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES



        ; create vertex buffer
        $mov ecx, 1024
        $mov edx, D3D12_HEAP_TYPE_DEFAULT
        $mov r8d, D3D12_RESOURCE_STATE_COPY_DEST
        $call create_buffer
        $mov [glob.mesh_vb], rax

        ; create index buffer
        $mov ecx, 1024
        $mov edx, D3D12_HEAP_TYPE_DEFAULT
        $mov r8d, D3D12_RESOURCE_STATE_COPY_DEST
        $call create_buffer
        $mov [glob.mesh_ib], rax


        ; upload vertex buffer data
        $mov ecx, 1024
        $call get_upload_buffer ; returns upload buffer in rax

        ; fill staging vertex buffer
        $vmovaps xmm0, [tri_v0]
        $vmovaps [rdx], xmm0
        $vmovaps xmm0, [tri_v1]
        $vmovaps [rdx+16], xmm0
        $vmovaps xmm0, [tri_v2]
        $vmovaps [rdx+32], xmm0

        $mov rcx, [glob.cmdlist]
        $mov rdx, [glob.mesh_vb]
        $mov r8d, 0
        $mov r9, rax            ; source upload buffer
        $mov [.param5], 0
        $mov [.param6], 1024
        $comcall ID3D12GraphicsCommandList.CopyBufferRegion


        ; upload index buffer data
        $mov ecx, 1024
        $call get_upload_buffer ; returns upload buffer in rax

        ; fill staging index buffer
        $mov [rdx], dword 0
        $mov [rdx+4], dword 1
        $mov [rdx+8], dword 2

        $mov rcx, [glob.cmdlist]
        $mov rdx, [glob.mesh_ib]
        $mov r8d, 0
        $mov r9, rax            ; source upload buffer
        $mov [.param5], 0
        $mov [.param6], 1024
        $comcall ID3D12GraphicsCommandList.CopyBufferRegion


        $transitionBarrier .barrier0, [glob.mesh_vb], D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER
        $transitionBarrier .barrier1, [glob.mesh_ib], D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER
        $mov rcx, [glob.cmdlist]
        $mov edx, 2
        $lea r8, [.barrier0]
        $comcall ID3D12GraphicsCommandList.ResourceBarrier


        ; create vertex buffer view
        $mov rcx, [glob.mesh_vb]
        $comcall ID3D12Resource.GetGPUVirtualAddress
        $mov [glob.mesh_vb_view.BufferLocation], rax
        $mov [glob.mesh_vb_view.SizeInBytes], 1024
        $mov [glob.mesh_vb_view.StrideInBytes], 16

        ; create index buffer view
        $mov rcx, [glob.mesh_ib]
        $comcall ID3D12Resource.GetGPUVirtualAddress
        $mov [glob.mesh_ib_view.BufferLocation], rax
        $mov [glob.mesh_ib_view.SizeInBytes], 1024
        $mov [glob.mesh_ib_view.Format], DXGI_FORMAT_R32_UINT


        ; create texture
        $lea rcx, [sz_img_gradient]
        $call create_texture_from_file
        $mov [glob.texture], rax

        $d3d12EncodeShader4ComponentMapping [.srv_desc.Shader4ComponentMapping], 0, 1, 2, 3
        $mov [.srv_desc.Format], DXGI_FORMAT_B8G8R8A8_UNORM
        $mov [.srv_desc.ViewDimension], D3D12_SRV_DIMENSION_TEXTURE2D
        $mov [.srv_desc.Texture2D.MipLevels], -1

        $mov rcx, [glob.device]
        $mov rdx, [glob.texture]
        $lea r8, [.srv_desc]
        $mov r9, [glob.cbv_srv_uav_heap_start]
        $comcall ID3D12Device.CreateShaderResourceView



        ; load VS
        $lea rcx, [sz_vs_object]
        $call load_file
        $mov [.pso_desc.VS.pShaderBytecode], rax
        $mov [.pso_desc.VS.BytecodeLength], rdx

        ; load PS
        $lea rcx, [sz_ps_object]
        $call load_file
        $mov [.pso_desc.PS.pShaderBytecode], rax
        $mov [.pso_desc.PS.BytecodeLength], rdx


        $lea [.input_element_desc0.SemanticName], rax, [sz_position]
        $mov [.input_element_desc0.SemanticIndex], 0
        $mov [.input_element_desc0.Format], DXGI_FORMAT_R32G32_FLOAT
        $mov [.input_element_desc0.InputSlot], 0
        $mov [.input_element_desc0.AlignedByteOffset], 0

        $lea [.input_element_desc1.SemanticName], rax, [sz_texcoord]
        $mov [.input_element_desc1.SemanticIndex], 0
        $mov [.input_element_desc1.Format], DXGI_FORMAT_R32G32_FLOAT
        $mov [.input_element_desc1.InputSlot], 0
        $mov [.input_element_desc1.AlignedByteOffset], 8


        $lea [.pso_desc.InputLayout.pInputElementDescs], rax, [.input_element_desc0]
        $mov [.pso_desc.InputLayout.NumElements], 2
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

        $free [.pso_desc.VS.pShaderBytecode]
        $free [.pso_desc.PS.pShaderBytecode]

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
  .k_stack_size = 32*1+24
        $sub rsp, .k_stack_size

        $safeRelease [glob.mesh_vb]
        $safeRelease [glob.mesh_ib]
        $safeRelease [glob.texture]
        $safeRelease [glob.pso]
        $safeRelease [glob.rootsig]

        $add rsp, .k_stack_size
        $ret
;=============================================================================
falign
scene1_update:
;-----------------------------------------------------------------------------
        $push rsi rdi r12
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 6 n { .param#n dq ? }
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
        $vmovss xmm0, [eye_half_fovy]
        $call fast_sincosf
        $vmovss xmm2, [glob.viewport.Width]
        $vdivss xmm2, xmm2, [glob.viewport.Height]
        $vmovss xmm3, [eye_nearz]
        $vmovss xmm4, [eye_farz]
        $matrixPerspectiveFov
        $vmovaps [.matrix], ymm0
        $vmovaps [.matrix+32], ymm1

        $vmovaps xmm0, [eye_position]
        $vmovaps xmm1, [eye_focus]
        $vmovaps xmm2, [eye_up]
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



        $mov rcx, [glob.device]
        $mov edx, 1
        $mov r8, [.frame_res.descriptor_heap_cpu_start]
        $mov r9, [glob.cbv_srv_uav_heap_start]
        $mov [.param5], D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
        $comcall ID3D12Device.CopyDescriptorsSimple


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

        ; set descriptor heap
        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $lea r8, [.frame_res.descriptor_heap]
        $comcall ID3D12GraphicsCommandList.SetDescriptorHeaps

        ; set root signature and resources
        $mov rcx, [glob.cmdlist]
        $mov rdx, [glob.rootsig]
        $comcall ID3D12GraphicsCommandList.SetGraphicsRootSignature

        $mov rcx, [glob.cmdlist]
        $mov edx, 0
        $mov r8, [.frame_res.constant_buffer_view]
        $comcall ID3D12GraphicsCommandList.SetGraphicsRootConstantBufferView

        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $mov r8, [.frame_res.descriptor_heap_gpu_start]
        $comcall ID3D12GraphicsCommandList.SetGraphicsRootDescriptorTable

        ; vertex buffer view
        $mov rcx, [glob.cmdlist]
        $mov edx, 0
        $mov r8d, 1
        $lea r9, [glob.mesh_vb_view]
        $comcall ID3D12GraphicsCommandList.IASetVertexBuffers

        ; index buffer view
        $mov rcx, [glob.cmdlist]
        $lea rdx, [glob.mesh_ib_view]
        $comcall ID3D12GraphicsCommandList.IASetIndexBuffer

        ; back buffer to D3D12_RESOURCE_STATE_RENDER_TARGET
        $mov eax, [glob.back_buffer_index]
        $transitionBarrier .barrier, [glob.swapchain_buffers+rax*8], D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET
        $mov rcx, [glob.cmdlist]
        $mov edx, 1
        $lea r8, [.barrier]
        $comcall ID3D12GraphicsCommandList.ResourceBarrier

        ; get back buffer handle
        $mov eax, [glob.back_buffer_index]
        $imul eax, [glob.rtv_size]
        $add rax, [glob.rtv_heap_start]
        $mov [.back_buffer_handle], rax

        ; clear back buffer
        $mov rcx, [glob.cmdlist]
        $mov rdx, [.back_buffer_handle]
        $lea r8, [clear_color]
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
        $mov r9d, 0
        $mov [.param5], 0
        $mov [.param6], 0
        $comcall ID3D12GraphicsCommandList.DrawIndexedInstanced

        ; back buffer to D3D12_RESOURCE_STATE_PRESENT
        $mov eax, [glob.back_buffer_index]
        $transitionBarrier .barrier, [glob.swapchain_buffers+rax*8], D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT
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
