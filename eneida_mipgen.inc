;=============================================================================
; in: rcx - mipmap_generator structure addr
; in: rdx - cpu descriptor handel for heap start
; in: r8 - gpu descriptor handel for heap start
falign
mipgen_init:
;-----------------------------------------------------------------------------
        $push r12 rsi
        $sub rsp, .k_stack_size
  virtual at rsp
  rept 8 n { .param#n dq ? }
  .barrier D3D12_RESOURCE_BARRIER
  dalign 8
  .upload_heap_props D3D12_HEAP_PROPERTIES
  dalign 8
  .texture_desc D3D12_RESOURCE_DESC
  dalign 8
  .uav_desc D3D12_UNORDERED_ACCESS_VIEW_DESC
  dalign 8
  .pso_desc D3D12_COMPUTE_PIPELINE_STATE_DESC
  dalign 32
  .k_stack_size = $-$$+8
  end virtual

        $mov r12, rcx
  virtual at r12
  .mipgen mipmap_generator
  end virtual

        $zeroStack .k_stack_size

        $mov [.mipgen.dheap_cpu_start], rdx
        $mov [.mipgen.dheap_gpu_start], r8

        $mov [.upload_heap_props.Type], D3D12_HEAP_TYPE_DEFAULT
        $mov [.texture_desc.Dimension], D3D12_RESOURCE_DIMENSION_TEXTURE2D
        $mov [.texture_desc.Width], 512
        $mov [.texture_desc.Height], 512
        $mov [.texture_desc.DepthOrArraySize], 1
        $mov [.texture_desc.MipLevels], 0
        $mov [.texture_desc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
        $mov [.texture_desc.SampleDesc.Count], 1
        $mov [.texture_desc.Layout], D3D12_TEXTURE_LAYOUT_UNKNOWN
        $mov [.texture_desc.Flags], D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS

        $mov [.uav_desc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
        $mov [.uav_desc.ViewDimension], D3D12_UAV_DIMENSION_TEXTURE2D

        $xor esi, esi
  .create_textures_loop:
        ; create texture
        $mov rcx, [glob.device]
        $lea rdx, [.upload_heap_props]
        $mov r8d, D3D12_HEAP_FLAG_NONE
        $lea r9, [.texture_desc]
        $mov [.param5], D3D12_RESOURCE_STATE_UNORDERED_ACCESS
        $mov [.param6], 0
        $lea [.param7], rax, [IID_ID3D12Resource]
        $lea [.param8], rax, [.mipgen.mip_textures+rsi*8]
        $comcall ID3D12Device.CreateCommittedResource
        $checkhr eax, .error

        ; create texture view
        $mov eax, esi
        $imul eax, [glob.cbv_srv_uav_size]
        $add rax, [.mipgen.dheap_cpu_start]

        $mov rcx, [glob.device]
        $mov rdx, [.mipgen.mip_textures+rsi*8]
        $mov r8d, 0
        $lea r9, [.uav_desc]
        $mov [.param5], rax
        $comcall ID3D12Device.CreateUnorderedAccessView

        $shr [.texture_desc.Width], 1
        $shr [.texture_desc.Height], 1
        $add esi, 1
        $cmp esi, 4
        $jne .create_textures_loop


        ; load CS
        $lea rcx, [sz_cs_mipgen]
        $call load_file
        $mov [.pso_desc.CS.pShaderBytecode], rax
        $mov [.pso_desc.CS.BytecodeLength], rdx

        $mov rcx, [glob.device]
        $lea rdx, [.pso_desc]
        $lea r8, [IID_ID3D12PipelineState]
        $lea r9, [.mipgen.pso]
        $comcall ID3D12Device.CreateComputePipelineState
        $checkhr eax, .error

        $mov rcx, [glob.device]
        $mov edx, 0
        $mov r8, [.pso_desc.CS.pShaderBytecode]
        $mov r9, [.pso_desc.CS.BytecodeLength]
        $lea [.param5], rax, [IID_ID3D12RootSignature]
        $lea [.param6], rax, [.mipgen.rootsig]
        $comcall ID3D12Device.CreateRootSignature
        $checkhr eax, .error


        $mov eax, 1
        $jmp .return
  .error:
        $xor eax, eax
  .return:
        $add rsp, .k_stack_size
        $pop rsi r12
        $ret

