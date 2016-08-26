format PE64 GUI 4.0
entry start
;==============================================================================
; Constants
;------------------------------------------------------------------------------
INFINITE = 0xffffffff
IDI_APPLICATION = 32512
IDC_ARROW = 32512
WS_VISIBLE = 010000000h
WS_OVERLAPPED = 000000000h
WS_CAPTION = 000C00000h
WS_SYSMENU = 000080000h
WS_VISIBLE = 010000000h
WS_MINIMIZEBOX = 000020000h
CW_USEDEFAULT = 80000000h
PM_REMOVE = 0001h
WM_QUIT = 0012h
WM_KEYDOWN = 0100h
WM_DESTROY = 0002h
VK_ESCAPE = 01Bh
SRCCOPY = 0x00CC0020
OPEN_EXISTING = 3
GENERIC_READ = 0x80000000
INVALID_HANDLE_VALUE = 0xffffffffffffffff
INVALID_FILE_SIZE = 0xffffffff
FILE_ATTRIBUTE_NORMAL = 128
FILE_FLAG_SEQUENTIAL_SCAN = 0x08000000
EVENT_ALL_ACCESS = 0x1F0003
;==============================================================================
; Macros
;------------------------------------------------------------------------------
macro maxValue dest, [v] {
  common
    maxv = 0
  forward
    if v > maxv
      maxv = v
    end if
  common
  dest = maxv }

macro emit [inst] {
  forward
            inst }

macro iacaBegin {
            mov ebx, 111
            db  0x64, 0x67, 0x90 }

macro iacaEnd {
            mov ebx, 222
            db  0x64, 0x67, 0x90 }

macro debugBreak { int3 }
macro falign { align 32 }
macro dalign value* { rb (value - 1) - (($-$$) + (value - 1)) mod value }

struc dw [v] {
  common
  dalign 2
  . dw v }

struc dd [v] {
  common
  dalign 4
  . dd v }

struc dq [v] {
  common
  dalign 8
  . dq v }

macro comcall target* {
            mov  rax, [rcx]
            call [rax+target] }

macro icall target* {
            call [target] }

macro malloc size* {
            mov   rcx, [glob.process_heap]
            xor   edx, edx
            mov   r8d, size
            icall HeapAlloc }

macro free ptr* {
  local ..end
            mov   r8, ptr
            test  r8, r8
            jz    ..end
            mov   rcx, [glob.process_heap]
            xor   edx, edx
            icall HeapFree
  ..end: }

macro safeClose handle* {
  local .end
            mov   rcx, handle
            test  rcx, rcx
            jz    .end
            icall CloseHandle
            mov   handle, 0
  .end: }

macro safeRelease iface* {
  local ..end
            mov  rcx, iface
            test rcx, rcx
            jz   ..end
            mov  rax, [rcx]
            call [IUnknown.Release+rax]
            mov  iface, 0
  ..end: }

macro transBar ptr*, res*, sbefore*, safter* {
            mov [ptr+D3D12_RESOURCE_BARRIER.Type], D3D12_RESOURCE_BARRIER_TYPE_TRANSITION
            mov [ptr+D3D12_RESOURCE_BARRIER.Transition.pResource], rax, res
            mov [ptr+D3D12_RESOURCE_BARRIER.Flags], 0
            mov [ptr+D3D12_RESOURCE_BARRIER.Transition.StateBefore], sbefore
            mov [ptr+D3D12_RESOURCE_BARRIER.Transition.StateAfter], safter
            mov [ptr+D3D12_RESOURCE_BARRIER.Transition.Subresource], D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES }

macro mov op1*, op2*, op3 {
  if op3 eq
            mov op1, op2
  else
            mov op2, op3
            mov op1, op2
  end if }

macro lea op1*, op2*, op3 {
  if op3 eq
            lea op1, op2
  else
            lea op2, op3
            mov op1, op2
  end if }

macro zeroStack size* {
            vpxor   ymm0, ymm0, ymm0
            xor     eax, eax
  @@:       vmovdqa [rsp+rax], ymm0
            add     eax, 32
            cmp     eax, 32*(size/32)
            jne     @b }

macro checkhr res*, err* {
            test res, res
            js   err }

macro vshufps d*, s0*, s1*, fp3*, fp2, fp1, fp0 {
  if fp2 eq
            vshufps d, s0, s1, fp3
  else
            vshufps d, s0, s1, (fp3 shl 6) or (fp2 shl 4) or (fp1 shl 2) or fp0
  end if }

macro strucOffsetsSize s* {
  virtual at 0
  s s
  sizeof.#s = $
  end virtual }
;==============================================================================
; Structures
;------------------------------------------------------------------------------
alignment.POINT = 4
struc POINT {
  dalign 4
  .:
  .x dd 0
  .y dd 0
  dalign 4 }
strucOffsetsSize POINT

struc MSG {
  dalign 8
  .:
  .hwnd    dq 0
  .message dd 0
  .wParam  dq 0
  .lParam  dq 0
  .time    dd 0
  .pt      POINT
  dalign 8 }
strucOffsetsSize MSG

struc WNDCLASS {
  dalign 8
  .:
  .style         dd 0
  .lpfnWndProc   dq 0
  .cbClsExtra    dd 0
  .cbWndExtra    dd 0
  .hInstance     dq 0
  .hIcon         dq 0
  .hCursor       dq 0
  .hbrBackground dq 0
  .lpszMenuName  dq 0
  .lpszClassName dq 0
  dalign 8 }
strucOffsetsSize WNDCLASS

struc RECT {
  dalign 4
  .:
  .left   dd 0
  .top    dd 0
  .right  dd 0
  .bottom dd 0
  dalign 4 }
strucOffsetsSize RECT

struc GUID p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 {
  dd p0
  dw p1
  dw p2
  db p3
  db p4
  db p5
  db p6
  db p7
  db p8
  db p9
  db p10 }
;========================================================================
include 'd3d12.inc'
;========================================================================
selected_scene fix scene1
macro sceneInit { call selected_scene#_init }
macro sceneDeinit { call selected_scene#_deinit }
macro sceneUpdate { call selected_scene#_update }

DEBUG                        equ 0
k_win_style                  equ WS_OVERLAPPED+WS_SYSMENU+WS_CAPTION+WS_MINIMIZEBOX
k_buffered_frames_count      equ 3
k_swapchain_buffers_count    equ 4
k_upload_buffers_count       equ 16
k_max_descriptors_count      equ 1000
;========================================================================
section '.data' data readable writeable

struc mipmap_generator {
  dalign 8
  .:
  .pso                  dq 0
  .rootsig              dq 0
  .mip_textures         dq 4 dup 0
  .dheap_cpu_start      dq 0
  .dheap_gpu_start      dq 0
  dalign 8 }
strucOffsetsSize mipmap_generator
;=============================================================================
; scene1 data
;-----------------------------------------------------------------------------
dalign 8
scene1:
  .mesh_vb              dq 0
  .mesh_ib              dq 0
  .texture              dq 0
  .pso                  dq 0
  .rootsig              dq 0

  .eye_half_fovy        dd 0.52359876 ; pi / 6
  .eye_nearz            dd 1.0
  .eye_farz             dd 100.0

  .clear_color          dd 0.0, 0.2, 0.4, 1.0

  dalign 16
  .eye_position:        dd 1.2, 1.2, -1.2, 1.0
  .eye_focus:           dd 0.0, 0.0, 0.0, 1.0
  .eye_up:              dd 0.0, 1.0, 0.0, 0.0

  .tri_v0:              dd -0.7, -0.7, 0.0, 1.0
  .tri_v1:              dd 0.0, 0.7, 0.5, 0.0
  .tri_v2:              dd 0.7, -0.7, 1.0, 1.0

  .mesh_vb_view         D3D12_VERTEX_BUFFER_VIEW
  .mesh_ib_view         D3D12_INDEX_BUFFER_VIEW
;=============================================================================
; global data
;-----------------------------------------------------------------------------
struc frame_resources {
  dalign 8
  .:
  .constant_buffer      dq 0
  .constant_buffer_addr dq 0
  .constant_buffer_view dq 0
  .cmdalloc             dq 0
  .dheap                dq 0
  .dheap_cpu_start      dq 0
  .dheap_gpu_start      dq 0
  dalign 8 }
strucOffsetsSize frame_resources

dalign 8
glob:
  .upload_buffers       dq k_upload_buffers_count dup 0
  .upload_buffers_count dd 0, 0

  .factory_dxgi         dq 0
  .device               dq 0
  .cmdqueue             dq 0
  .cmdlist              dq 0

  .swapchain            dq 0
  .swapchain_buffers    dq k_swapchain_buffers_count dup 0

  .rtv_heap               dq 0
  .rtv_heap_start         dq 0
  .cbv_srv_uav_heap       dq 0
  .cbv_srv_uav_heap_start dq 0

  .frame_fence          dq 0
  .frame_fence_event    dq 0
  .cpu_completed_fences dq 0

  .viewport   D3D12_VIEWPORT
  .scissor    D3D12_RECT

  .rtv_size             dd 0
  .cbv_srv_uav_size     dd 0
  .back_buffer_index    dd 0
  .frame_index          dd 0

  rept k_buffered_frames_count n:0 { .frame_res#n frame_resources }

  .win_handle           dq 0
  .win_width            dd 1280
  .win_height           dd 720
  .process_heap         dq 0
  .time                 dq 0
  .time_delta           dd 0
;=============================================================================
match = 1, DEBUG {
  output_debug_string rb 256 }

sz_position             db 'POSITION', 0
sz_texcoord             db 'TEXCOORD', 0
sz_vs_object            db 'data/shader/object_vs.cso', 0
sz_ps_object            db 'data/shader/object_ps.cso', 0
sz_img_gradient         db 'data/image/ceiling_GRosinWoodDirty_256_d.tga', 0
sz_cs_mipgen            db 'data/shader/mipgen_cs.cso', 0

get_time.perf_counter               dq 0
get_time.perf_freq                  dq 0
get_time.first_perf_counter         dq 0

update_frame_stats.prev_time        dq 0
update_frame_stats.prev_update_time dq 0
update_frame_stats.frame            dd 0, 0

k_f64_1000000_0         dq 1000000.0
k_f64_1_0               dq 1.0

k_win_class_name        db 'eneida', 0
k_win_text_fmt          db '[%d fps  %d us] eneida', 0

align 8
IID_IDXGISwapChain3                 GUID 0x94d99bdb,0xf1f8,0x4ab0,0xb2,0x36,0x7d,0xa0,0x17,0x0e,0xda,0xb1
IID_IDXGIFactory4                   GUID 0x1bc6ea02,0xef36,0x464f,0xbf,0x0c,0x21,0xca,0x39,0xe5,0x16,0x8a
IID_ID3D12Device                    GUID 0x189819f1,0x1db6,0x4b57,0xbe,0x54,0x18,0x21,0x33,0x9b,0x85,0xf7
IID_ID3D12Debug                     GUID 0x344488b7,0x6846,0x474b,0xb9,0x89,0xf0,0x27,0x44,0x82,0x45,0xe0
IID_ID3D12CommandQueue              GUID 0x0ec870a6,0x5d7e,0x4c22,0x8c,0xfc,0x5b,0xaa,0xe0,0x76,0x16,0xed
IID_ID3D12CommandAllocator          GUID 0x6102dee4,0xaf59,0x4b09,0xb9,0x99,0xb4,0x4d,0x73,0xf0,0x9b,0x24
IID_ID3D12CommandList               GUID 0x7116d91c,0xe7e4,0x47ce,0xb8,0xc6,0xec,0x81,0x68,0xf4,0x37,0xe5
IID_ID3D12GraphicsCommandList       GUID 0x5b160d0f,0xac1b,0x4185,0x8b,0xa8,0xb3,0xae,0x42,0xa5,0xa4,0x55
IID_ID3D12DescriptorHeap            GUID 0x8efb471d,0x616c,0x4f49,0x90,0xf7,0x12,0x7b,0xb7,0x63,0xfa,0x51
IID_ID3D12Resource                  GUID 0x696442be,0xa72e,0x4059,0xbc,0x79,0x5b,0x5c,0x98,0x04,0x0f,0xad
IID_ID3D12Fence                     GUID 0x0a753dcf,0xc4d8,0x4b91,0xad,0xf6,0xbe,0x5a,0x60,0xd9,0x5a,0x76
IID_ID3D12RootSignature             GUID 0xc54a6b66,0x72df,0x4ee8,0x8b,0xe5,0xa9,0x46,0xa1,0x42,0x92,0x14
IID_ID3D12PipelineState             GUID 0x765a30f3,0xf624,0x4c6f,0xa8,0x28,0xac,0xe9,0x48,0x62,0x24,0x45

; math constants
align 64
k_f32_identity_r0:      dd 1.0, 0.0, 0.0, 0.0
k_f32_identity_r1:      dd 0.0, 1.0, 0.0, 0.0
k_f32_identity_r2:      dd 0.0, 0.0, 1.0, 0.0
k_f32_identity_r3:      dd 0.0, 0.0, 0.0, 1.0
k_f32_pi:               dd 8 dup 3.141592654
k_f32_two_pi:           dd 8 dup 6.283185307
k_f32_recip_two_pi:     dd 8 dup 0.159154943
k_f32_half_pi:          dd 8 dup 1.570796327
k_f32_half:             dd 8 dup 0.5
k_f32_sin_coefficients: dd -0.16666667, 0.0083333310, -0.00019840874, 2.7525562e-06, -2.3889859e-08, -0.16665852, 0.0083139502, -0.00018524670
k_f32_cos_coefficients: dd -0.5, 0.041666638, -0.0013888378, 2.4760495e-05, -2.6051615e-07, -0.49992746, 0.041493919, -0.0012712436
k_f32_one:              dd 8 dup 1.0
k_f32_negative_one:     dd 8 dup -1.0
k_i32_negative_zero:    dd 8 dup 0x80000000
k_i32_negative_zero_x0: dd 0x80000000, 0, 0, 0, 0, 0, 0, 0
k_i32_negative_zero_y0: dd 0, 0x80000000, 0, 0, 0, 0, 0, 0
k_i32_negative_zero_z0: dd 0, 0, 0x80000000, 0, 0, 0, 0, 0
k_i32_perm_x0_y1:       dd 0, 0, 0, 0, 5, 5, 5, 5
k_i32_perm_z0_w1:       dd 2, 2, 2, 2, 7, 7, 7, 7
k_i32_perm_y0_x1:       dd 1, 1, 1, 1, 4, 4, 4, 4
k_i32_perm_w0_z1:       dd 3, 3, 3, 3, 6, 6, 6, 6
;=============================================================================
section '.text' code readable executable

include 'eneida_math.inc'
include 'eneida_demo.inc'
include 'eneida_scene1.inc'
include 'eneida_lib.inc'
include 'eneida_mipgen.inc'
;=============================================================================
falign
check_cpu_extensions:
;-----------------------------------------------------------------------------
            mov         eax, 1
            cpuid
            and         ecx, 0x58001000          ; check RDRAND, AVX, OSXSAVE, FMA
            cmp         ecx, 0x58001000
            jne         .not_supported
            mov         eax, 7
            xor         ecx, ecx
            cpuid
            and         ebx, 0x20                ; check AVX2
            cmp         ebx, 0x20
            jne         .not_supported
            xor         ecx, ecx
            xgetbv
            and         eax, 0x6                 ; check OS support
            cmp         eax, 0x6
            jne         .not_supported
            mov         eax, 1
            ret
  .not_supported:
            xor         eax, eax
            ret
;=============================================================================
falign
update_frame_stats:
;-----------------------------------------------------------------------------
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 4 n:1 { .param#n dq ? }

  .text rb 64

  dalign 32
  .k_stack_size = $-$$+24
  end virtual
  ;---------------------------------------
            mov         rax, [.prev_time]
            test        rax, rax
            jnz         @f

            call        get_time
            vmovsd      [.prev_time], xmm0
            vmovsd      [.prev_update_time], xmm0

  @@:       call        get_time                            ; xmm0 = (0,time)
            vmovsd      [glob.time], xmm0
            vsubsd      xmm1, xmm0, [.prev_time]            ; xmm1 = (0,time_delta)
            vmovsd      [.prev_time], xmm0
            vxorps      xmm2, xmm2, xmm2
            vcvtsd2ss   xmm1, xmm2, xmm1                    ; xmm1 = (0,0,0,time_delta)
            vmovss      [glob.time_delta], xmm1
            vmovsd      xmm1, [.prev_update_time]           ; xmm1 = (0,prev_update_time)
            vsubsd      xmm2, xmm0, xmm1                    ; xmm2 = (0,time-prev_update_time)
            vcomisd     xmm2, [k_f64_1_0]
            jb          @f

            vmovsd      [.prev_update_time], xmm0
            mov         eax, [.frame]
            vxorpd      xmm1, xmm1, xmm1
            vcvtsi2sd   xmm1, xmm1, eax                     ; xmm1 = (0,frame)
            vdivsd      xmm0, xmm1, xmm2                    ; xmm0 = (0,frame/(time-prev_update_time))
            vdivsd      xmm1, xmm2, xmm1
            vmulsd      xmm1, xmm1, [k_f64_1000000_0]
            mov         [.frame], 0

            lea         rcx, [.text]
            lea         rdx, [k_win_text_fmt]
            vcvtsd2si   r8, xmm0
            vcvtsd2si   r9, xmm1
            icall       wsprintf
            mov         rcx, [glob.win_handle]
            lea         rdx, [.text]
            icall       SetWindowText

  @@:       add         [.frame], 1
            add         rsp, .k_stack_size
            ret
;=============================================================================
falign
init_window:
;-----------------------------------------------------------------------------
            push        rsi
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 12 n:1 { .param#n dq ? }

  .wc   WNDCLASS
  .rect RECT

  dalign 32
  .k_stack_size = $-$$+16
  end virtual
  ;---------------------------------------
            zeroStack   .k_stack_size
            ; create window class
            lea         [.wc.lpfnWndProc], rax, [winproc]
            lea         [.wc.lpszClassName], rax, [k_win_class_name]
            xor         ecx, ecx
            icall       GetModuleHandle
            mov         [.wc.hInstance], rax
            xor         ecx, ecx
            mov         edx, IDC_ARROW
            icall       LoadCursor
            mov         [.wc.hCursor], rax
            lea         rcx, [.wc]
            icall       RegisterClass
            test        eax, eax
            jz          .error
            ; compute window size
            mov         [.rect.right], eax, [glob.win_width]
            mov         [.rect.bottom], eax, [glob.win_height]
            lea         rcx, [.rect]
            mov         edx, k_win_style
            xor         r8d, r8d
            icall       AdjustWindowRect
            mov         r10d, [.rect.right]
            mov         r11d, [.rect.bottom]
            sub         r10d, [.rect.left]
            sub         r11d, [.rect.top]
            ; create window
            xor         ecx, ecx
            lea         rdx, [k_win_class_name]
            mov         r8, rdx
            mov         r9d, WS_VISIBLE+k_win_style
            mov         dword[.param5], CW_USEDEFAULT
            mov         dword[.param6], CW_USEDEFAULT
            mov         dword[.param7], r10d
            mov         dword[.param8], r11d
            mov         [.param9], 0
            mov         [.param10], 0
            mov         [.param11], rax, [.wc.hInstance]
            mov         [.param12], 0
            icall       CreateWindowEx
            mov         [glob.win_handle], rax
            test        rax, rax
            jz          .error
            ; success
            mov         eax, 1
            jmp         .return
  .error:   xor         eax, eax
  .return:  add         rsp, .k_stack_size
            pop         rsi
            ret
;=============================================================================
falign
init:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
            sub         rsp, .k_stack_size
            ; check cpu
            call        check_cpu_extensions
            test        eax, eax
            jz          .error
            ; get process heap
            icall       GetProcessHeap
            mov         [glob.process_heap], rax
            test        rax, rax
            jz          .error
            ; create window
            call        init_window
            test        eax, eax
            jz          .error
            ; init demo
            call        demo_init
            test        eax, eax
            jz          .error
            ; success
            mov         eax, 1
            jmp         .return
  .error:   xor         eax, eax
  .return:  add         rsp, .k_stack_size
            ret
;=============================================================================
falign
deinit:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
            sub         rsp, .k_stack_size
            call        wait_for_gpu
            call        demo_deinit
            add         rsp, .k_stack_size
            ret
;=============================================================================
falign
update:
;-----------------------------------------------------------------------------
  .k_stack_size = 32*1+24
            sub         rsp, .k_stack_size
            call        update_frame_stats
            call        demo_update
            add         rsp, .k_stack_size
            ret
;=============================================================================
falign
start:
;-----------------------------------------------------------------------------
            and         rsp, -32
            sub         rsp, .k_stack_size
  ;---------------------------------------
  virtual at rsp
  rept 5 n { .param#n dq ? }

  .msg MSG

  dalign 32
  .k_stack_size = $-$$
  end virtual
  ;---------------------------------------
            call        init
            test        eax, eax
            jz          .quit
  .main_loop:
            lea         rcx, [.msg]
            xor         edx, edx
            xor         r8d, r8d
            xor         r9d, r9d
            mov         dword[.param5], PM_REMOVE
            icall       PeekMessage
            test        eax, eax
            jz          .update

            lea         rcx, [.msg]
            icall       DispatchMessage
            cmp         [.msg.message], WM_QUIT
            je          .quit

            jmp         .main_loop
    .update:
            call        update
            test        eax, eax
            jz          .quit
            jmp         .main_loop
            .quit:
            call        deinit
            xor         ecx, ecx
            icall       ExitProcess
;=============================================================================
falign
winproc:
;-----------------------------------------------------------------------------
  .k_stack_size = 16*2+8
            sub         rsp, .k_stack_size
            cmp         edx, WM_KEYDOWN
            je          .keydown
            cmp         edx, WM_DESTROY
            je          .destroy
            icall       DefWindowProc
            jmp         .return
  .keydown:
            cmp         r8d, VK_ESCAPE
            jne         .return
            xor         ecx, ecx
            icall       PostQuitMessage
            xor         eax, eax
            jmp         .return
  .destroy:
            xor         ecx, ecx
            icall       PostQuitMessage
            xor         eax, eax
  .return:
            add         rsp, .k_stack_size
            ret
;========================================================================
section '.idata' import data readable writeable

dd 0, 0, 0, rva _kernel32, rva _kernel32_table
dd 0, 0, 0, rva _user32,   rva _user32_table
dd 0, 0, 0, rva _dxgi,     rva _dxgi_table
dd 0, 0, 0, rva _d3d12,    rva _d3d12_table
dd 0, 0, 0, 0, 0

dalign 8
_kernel32_table:
  GetModuleHandle           dq rva _GetModuleHandle
  ExitProcess               dq rva _ExitProcess
  ExitThread                dq rva _ExitThread
  QueryPerformanceFrequency dq rva _QueryPerformanceFrequency
  QueryPerformanceCounter   dq rva _QueryPerformanceCounter
  CloseHandle               dq rva _CloseHandle
  Sleep                     dq rva _Sleep
  LoadLibrary               dq rva _LoadLibrary
  FreeLibrary               dq rva _FreeLibrary
  GetProcAddress            dq rva _GetProcAddress
  HeapAlloc                 dq rva _HeapAlloc
  HeapReAlloc               dq rva _HeapReAlloc
  HeapFree                  dq rva _HeapFree
  CreateFile                dq rva _CreateFile
  ReadFile                  dq rva _ReadFile
  GetFileSize               dq rva _GetFileSize
  GetProcessHeap            dq rva _GetProcessHeap
  CreateEventEx             dq rva _CreateEventEx
  CreateThread              dq rva _CreateThread
  SetEvent                  dq rva _SetEvent
  WaitForSingleObject       dq rva _WaitForSingleObject
  WaitForMultipleObjects    dq rva _WaitForMultipleObjects
  OutputDebugString         dq rva _OutputDebugString
  RtlCopyMemory             dq rva _RtlCopyMemory
  RtlFillMemory             dq rva _RtlFillMemory
  RtlMoveMemory             dq rva _RtlMoveMemory
  RtlZeroMemory             dq rva _RtlZeroMemory
                            dq 0

dalign 8
_user32_table:
  wsprintf                  dq rva _wsprintf
  RegisterClass             dq rva _RegisterClass
  CreateWindowEx            dq rva _CreateWindowEx
  DefWindowProc             dq rva _DefWindowProc
  PeekMessage               dq rva _PeekMessage
  DispatchMessage           dq rva _DispatchMessage
  LoadCursor                dq rva _LoadCursor
  SetWindowText             dq rva _SetWindowText
  AdjustWindowRect          dq rva _AdjustWindowRect
  GetDC                     dq rva _GetDC
  ReleaseDC                 dq rva _ReleaseDC
  PostQuitMessage           dq rva _PostQuitMessage
  MessageBox                dq rva _MessageBox
                            dq 0

dalign 8
_dxgi_table:
  CreateDXGIFactory1        dq rva _CreateDXGIFactory1
                            dq 0

dalign 8
_d3d12_table:
  D3D12CreateDevice         dq rva _D3D12CreateDevice
  D3D12GetDebugInterface    dq rva _D3D12GetDebugInterface
                            dq 0

_kernel32                   db 'kernel32.dll', 0
_user32                     db 'user32.dll', 0
_gdi32                      db 'gdi32.dll', 0
_dxgi                       db 'dxgi.dll', 0
_d3d12                      db 'd3d12.dll', 0

emit <_GetModuleHandle           dw 0>, <db 'GetModuleHandleA', 0>
emit <_ExitProcess               dw 0>, <db 'ExitProcess', 0>
emit <_ExitThread                dw 0>, <db 'ExitThread', 0>
emit <_QueryPerformanceFrequency dw 0>, <db 'QueryPerformanceFrequency', 0>
emit <_QueryPerformanceCounter   dw 0>, <db 'QueryPerformanceCounter', 0>
emit <_CloseHandle               dw 0>, <db 'CloseHandle', 0>
emit <_Sleep                     dw 0>, <db 'Sleep', 0>
emit <_LoadLibrary               dw 0>, <db 'LoadLibraryA', 0>
emit <_FreeLibrary               dw 0>, <db 'FreeLibrary', 0>
emit <_GetProcAddress            dw 0>, <db 'GetProcAddress', 0>
emit <_HeapAlloc                 dw 0>, <db 'HeapAlloc', 0>
emit <_HeapReAlloc               dw 0>, <db 'HeapReAlloc', 0>
emit <_HeapFree                  dw 0>, <db 'HeapFree', 0>
emit <_CreateFile                dw 0>, <db 'CreateFileA', 0>
emit <_ReadFile                  dw 0>, <db 'ReadFile', 0>
emit <_GetFileSize               dw 0>, <db 'GetFileSize', 0>
emit <_GetProcessHeap            dw 0>, <db 'GetProcessHeap', 0>
emit <_CreateEventEx             dw 0>, <db 'CreateEventExA', 0>
emit <_CreateThread              dw 0>, <db 'CreateThread', 0>
emit <_SetEvent                  dw 0>, <db 'SetEvent', 0>
emit <_WaitForSingleObject       dw 0>, <db 'WaitForSingleObject', 0>
emit <_WaitForMultipleObjects    dw 0>, <db 'WaitForMultipleObjects', 0>
emit <_OutputDebugString         dw 0>, <db 'OutputDebugStringA', 0>
emit <_RtlCopyMemory             dw 0>, <db 'RtlCopyMemory', 0>
emit <_RtlFillMemory             dw 0>, <db 'RtlFillMemory', 0>
emit <_RtlMoveMemory             dw 0>, <db 'RtlMoveMemory', 0>
emit <_RtlZeroMemory             dw 0>, <db 'RtlZeroMemory', 0>

emit <_wsprintf                  dw 0>, <db 'wsprintfA', 0>
emit <_RegisterClass             dw 0>, <db 'RegisterClassA', 0>
emit <_CreateWindowEx            dw 0>, <db 'CreateWindowExA', 0>
emit <_DefWindowProc             dw 0>, <db 'DefWindowProcA', 0>
emit <_PeekMessage               dw 0>, <db 'PeekMessageA', 0>
emit <_DispatchMessage           dw 0>, <db 'DispatchMessageA', 0>
emit <_LoadCursor                dw 0>, <db 'LoadCursorA', 0>
emit <_SetWindowText             dw 0>, <db 'SetWindowTextA', 0>
emit <_AdjustWindowRect          dw 0>, <db 'AdjustWindowRect', 0>
emit <_GetDC                     dw 0>, <db 'GetDC', 0>
emit <_ReleaseDC                 dw 0>, <db 'ReleaseDC', 0>
emit <_PostQuitMessage           dw 0>, <db 'PostQuitMessage', 0>
emit <_MessageBox                dw 0>, <db 'MessageBoxA', 0>
    
emit <_CreateDXGIFactory1        dw 0>, <db 'CreateDXGIFactory1', 0>
            
emit <_D3D12CreateDevice         dw 0>, <db 'D3D12CreateDevice', 0>
emit <_D3D12GetDebugInterface    dw 0>, <db 'D3D12GetDebugInterface', 0>
;========================================================================

