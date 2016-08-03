;=============================================================================
; in: rcx - mipmap_generator structure addr
; in: rdx - cpu descriptor handel for heap start
; in: r8 - gpu descriptor handel for heap start
falign
mipgen_init:
;-----------------------------------------------------------------------------
            push        r12 rsi
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 8 n { .param#n dq ? }

  dalign 8, .upload_heap_props D3D12_HEAP_PROPERTIES
  dalign 8, .texture_desc      D3D12_RESOURCE_DESC
  dalign 8, .uav_desc          D3D12_UNORDERED_ACCESS_VIEW_DESC
  dalign 8, .pso_desc          D3D12_COMPUTE_PIPELINE_STATE_DESC

  dalign 32, .k_stack_size = $-$$+8
  end virtual

  virtual at r12
  .mipgen mipmap_generator
  end virtual
  ;---------------------------------------
            zeroStack   .k_stack_size

            mov         r12, rcx
            mov         [.mipgen.dheap_cpu_start], rdx
            mov         [.mipgen.dheap_gpu_start], r8

            mov         [.upload_heap_props.Type], D3D12_HEAP_TYPE_DEFAULT
            mov         [.texture_desc.Dimension], D3D12_RESOURCE_DIMENSION_TEXTURE2D
            mov         [.texture_desc.Width], 512
            mov         [.texture_desc.Height], 512
            mov         [.texture_desc.DepthOrArraySize], 1
            mov         [.texture_desc.MipLevels], 0
            mov         [.texture_desc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
            mov         [.texture_desc.SampleDesc.Count], 1
            mov         [.texture_desc.Layout], D3D12_TEXTURE_LAYOUT_UNKNOWN
            mov         [.texture_desc.Flags], D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS

            mov         [.uav_desc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
            mov         [.uav_desc.ViewDimension], D3D12_UAV_DIMENSION_TEXTURE2D

            xor         esi, esi

  .create_uavs_loop:
            ; create texture
            mov         rcx, [glob.device]
            lea         rdx, [.upload_heap_props]
            mov         r8d, D3D12_HEAP_FLAG_NONE
            lea         r9, [.texture_desc]
            mov         [.param5], D3D12_RESOURCE_STATE_UNORDERED_ACCESS
            mov         [.param6], 0
            lea         [.param7], rax, [IID_ID3D12Resource]
            lea         [.param8], rax, [.mipgen.mip_textures+rsi*8]
            comcall     ID3D12Device.CreateCommittedResource
            checkhr     eax, .error

            ; create texture view
            mov         eax, esi
            add         eax, 1
            imul        eax, [glob.cbv_srv_uav_size]
            add         rax, [.mipgen.dheap_cpu_start]

            mov         rcx, [glob.device]
            mov         rdx, [.mipgen.mip_textures+rsi*8]
            mov         r8d, 0
            lea         r9, [.uav_desc]
            mov         [.param5], rax
            comcall     ID3D12Device.CreateUnorderedAccessView

            shr         [.texture_desc.Width], 1
            shr         [.texture_desc.Height], 1
            add         esi, 1
            cmp         esi, 4
            jne         .create_uavs_loop


            ; create CS
            lea         [.pso_desc.CS.pShaderBytecode], rax, [cs_mipgen]
            mov         [.pso_desc.CS.BytecodeLength], rax, cs_mipgen.size

            mov         rcx, [glob.device]
            lea         rdx, [.pso_desc]
            lea         r8, [IID_ID3D12PipelineState]
            lea         r9, [.mipgen.pso]
            comcall     ID3D12Device.CreateComputePipelineState
            checkhr     eax, .error

            mov         rcx, [glob.device]
            mov         edx, 0
            mov         r8, [.pso_desc.CS.pShaderBytecode]
            mov         r9, [.pso_desc.CS.BytecodeLength]
            lea         [.param5], rax, [IID_ID3D12RootSignature]
            lea         [.param6], rax, [.mipgen.rootsig]
            comcall     ID3D12Device.CreateRootSignature
            checkhr     eax, .error


            mov         eax, 1
            jmp         .return
  .error:   xor         eax, eax
  .return:  add         rsp, .k_stack_size
            pop         rsi r12
            ret
;=============================================================================
; in: rcx - mipmap_generator structure addr
; in: rdx - command list
; in: r8 - texture resource
falign
mipgen_record_commands:
;-----------------------------------------------------------------------------
            push         r12 rbx rsi rdi
            sub          rsp, .k_stack_size
  ;----------------------------------------
  virtual at rsp
  rept 8 n:1 { .param#n dq ? }

  .texture        dq ?
  .cmdlist        dq ?
  .total_num_mips dd ?                    ; total number of mipmaps to generate
  .num_mips       dd ?                    ; number of mipmaps to generate in current iteration
  .src_mip        dd ?, ?

  dalign 8, rept 5 n:0 { .barrier\#n D3D12_RESOURCE_BARRIER }

  dalign 8, .texture_desc D3D12_RESOURCE_DESC
  dalign 8, .srv_desc     D3D12_SHADER_RESOURCE_VIEW_DESC
  dalign 8, .copy_src     D3D12_TEXTURE_COPY_LOCATION
  dalign 8, .copy_dst     D3D12_TEXTURE_COPY_LOCATION
  dalign 8, .src_box      D3D12_BOX

  dalign 32, .k_stack_size = $-$$+24
  end virtual

  virtual at r12
  .mipgen mipmap_generator
  end virtual
  ;---------------------------------------
            zeroStack   .k_stack_size

            ; save function inputs
            mov         r12, rcx
            mov         [.cmdlist], rdx
            mov         [.texture], r8

            mov         rcx, r8
            lea         rdx, [.texture_desc]
            comcall     ID3D12Resource.GetDesc

            d3d12EncodeShader4ComponentMapping [.srv_desc.Shader4ComponentMapping], 0, 1, 2, 3
            mov         [.srv_desc.Format], DXGI_FORMAT_R8G8B8A8_UNORM
            mov         [.srv_desc.ViewDimension], D3D12_SRV_DIMENSION_TEXTURE2D
            mov         [.srv_desc.Texture2D.MipLevels], -1

            mov         rcx, [glob.device]
            mov         rdx, [.texture]
            lea         r8, [.srv_desc]
            mov         r9, [.mipgen.dheap_cpu_start]
            comcall     ID3D12Device.CreateShaderResourceView

            mov         rcx, [.cmdlist]
            mov         rdx, [.mipgen.pso]
            comcall     ID3D12GraphicsCommandList.SetPipelineState

            mov         rcx, [.cmdlist]
            mov         rdx, [.mipgen.rootsig]
            comcall     ID3D12GraphicsCommandList.SetComputeRootSignature

            mov         rcx, [.cmdlist]
            mov         edx, 1
            mov         r8, [.mipgen.dheap_gpu_start]
            comcall     ID3D12GraphicsCommandList.SetComputeRootDescriptorTable

            movzx       eax, word[.texture_desc.MipLevels]
            sub         eax, 1
            mov         [.total_num_mips], eax
            mov         [.src_mip], 0

  .generation_loop:
            ; compute number of mipmaps to generate in this iteration
            mov         edx, 4                     ; edx = 4
            mov         eax, [.total_num_mips]
            cmp         eax, edx
            cmova       eax, edx
            test        eax, eax
            jz          .generation_loop_end
            mov         [.num_mips], eax


            mov         rcx, [.cmdlist]
            mov         edx, 0
            mov         r8d, [.src_mip]
            mov         r9d, 0
            comcall     ID3D12GraphicsCommandList.SetComputeRoot32BitConstant

            mov         rcx, [.cmdlist]
            mov         edx, 0
            mov         r8d, [.num_mips]
            mov         r9d, 1
            comcall     ID3D12GraphicsCommandList.SetComputeRoot32BitConstant

            mov         ecx, [.src_mip]
            add         ecx, 4
            mov         edx, dword[.texture_desc.Width]
            shr         edx, cl
            mov         r8d, [.texture_desc.Height]
            shr         r8d, cl
            mov         r9d, 1
            mov         rcx, [.cmdlist]
            comcall     ID3D12GraphicsCommandList.Dispatch


            transBar    .barrier0, [.mipgen.mip_textures], D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE
            transBar    .barrier1, [.mipgen.mip_textures+8], D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE
            transBar    .barrier2, [.mipgen.mip_textures+16], D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE
            transBar    .barrier3, [.mipgen.mip_textures+24], D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE
            transBar    .barrier4, [.texture], D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COPY_DEST

            mov         rcx, [.cmdlist]
            mov         edx, 5
            lea         r8, [.barrier0]
            comcall     ID3D12GraphicsCommandList.ResourceBarrier


            xor         ebx, ebx
  .copy_loop:
            lea         rcx, [.copy_dst]
            mov         edx, sizeof.D3D12_TEXTURE_COPY_LOCATION
            icall       RtlZeroMemory

            lea         rcx, [.copy_src]
            mov         edx, sizeof.D3D12_TEXTURE_COPY_LOCATION
            icall       RtlZeroMemory

            mov         [.copy_dst.pResource], rdx, [.texture]
            mov         [.copy_dst.Type], D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX
            mov         eax, [.src_mip]
            add         eax, 1
            add         eax, ebx
            mov         [.copy_dst.SubresourceIndex], eax

            mov         [.copy_src.pResource], rdx, [.mipgen.mip_textures+rbx*8]
            mov         [.copy_src.Type], D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX
            mov         [.copy_src.SubresourceIndex], 0

            mov         ecx, [.copy_dst.SubresourceIndex]
            mov         eax, dword[.texture_desc.Width]
            shr         eax, cl
            mov         [.src_box.right], eax
            mov         eax, [.texture_desc.Height]
            shr         eax, cl
            mov         [.src_box.bottom], eax
            mov         [.src_box.back], 1

            mov         rcx, [.cmdlist]
            lea         rdx, [.copy_dst]
            mov         r8d, 0
            mov         r9d, 0
            mov         [.param5], 0
            lea         [.param6], rax, [.copy_src]
            lea         [.param7], rax, [.src_box]
            comcall     ID3D12GraphicsCommandList.CopyTextureRegion

            add         ebx, 1
            cmp         ebx, [.num_mips]
            jne         .copy_loop


            transBar    .barrier0, [.mipgen.mip_textures], D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_UNORDERED_ACCESS
            transBar    .barrier1, [.mipgen.mip_textures+8], D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_UNORDERED_ACCESS
            transBar    .barrier2, [.mipgen.mip_textures+16], D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_UNORDERED_ACCESS
            transBar    .barrier3, [.mipgen.mip_textures+24], D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_UNORDERED_ACCESS
            transBar    .barrier4, [.texture], D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE

            mov         rcx, [.cmdlist]
            mov         edx, 5
            lea         r8, [.barrier0]
            comcall     ID3D12GraphicsCommandList.ResourceBarrier


            mov         eax, [.num_mips]
            sub         [.total_num_mips], eax
            add         [.src_mip], eax
            jmp         .generation_loop
  .generation_loop_end:


match = 1, DEBUG {
  local ..txt_fmt, ..end
            lea         rcx, [output_debug_string]
            lea         rdx, [..txt_fmt]
            mov         r8, [.texture_desc.Width]
            mov         r9d, [.texture_desc.Height]
            icall       wsprintf
            lea         rcx, [output_debug_string]
            icall       OutputDebugString
            jmp         ..end
  ..txt_fmt db 'eneida: width: %d  height: %d', 0
  ..end: }
            add         rsp, .k_stack_size
            pop         rdi rsi r12 rbx
            ret
;=============================================================================
; vim: ft=fasm ts=12 sts=12 sw=12 autoindent :
