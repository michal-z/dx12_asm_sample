;=============================================================================
falign
fast_sinf:
;-----------------------------------------------------------------------------
            vmovaps     xmm1, xmm0
            vmulss      xmm0, xmm0, [k_f32_recip_two_pi]
            vroundss    xmm0, xmm0, xmm0, 0
            vmulss      xmm0, xmm0, [k_f32_two_pi]
            vsubss      xmm0, xmm1, xmm0
            vandps      xmm1, xmm0, [k_i32_negative_zero]
            vorps       xmm2, xmm1, [k_f32_pi]
            vandnps     xmm3, xmm1, xmm0
            vsubss      xmm4, xmm2, xmm0
            vcmpless    xmm5, xmm3, [k_f32_half_pi]
            vblendvps   xmm0, xmm4, xmm0, xmm5
            vmulss      xmm1, xmm0, xmm0
            vmovss      xmm2, [k_f32_sin_coefficients+16]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients+12]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients+8]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients+4]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients]
            vfmadd213ss xmm2, xmm1, [k_f32_one]
            vmulss      xmm0, xmm2, xmm0
            ret
;=============================================================================
falign
fast_sin8f:
;-----------------------------------------------------------------------------
            vmovaps     ymm1, ymm0
            vmulps      ymm0, ymm0, [k_f32_recip_two_pi]
            vroundps    ymm0, ymm0, 0
            vmulps      ymm0, ymm0, [k_f32_two_pi]
            vsubps      ymm0, ymm1, ymm0
            vandps      ymm1, ymm0, [k_i32_negative_zero]
            vorps       ymm2, ymm1, [k_f32_pi]
            vandnps     ymm3, ymm1, ymm0
            vsubps      ymm4, ymm2, ymm0
            vcmpleps    ymm5, ymm3, [k_f32_half_pi]
            vblendvps   ymm0, ymm4, ymm0, ymm5
            vmulps      ymm1, ymm0, ymm0
            vbroadcastss ymm2, [k_f32_sin_coefficients+16]
            vbroadcastss ymm3, [k_f32_sin_coefficients+12]
            vbroadcastss ymm4, [k_f32_sin_coefficients+8]
            vbroadcastss ymm5, [k_f32_sin_coefficients+4]
            vbroadcastss ymm6, [k_f32_sin_coefficients]
            vfmadd213ps ymm2, ymm1, ymm3
            vfmadd213ps ymm2, ymm1, ymm4
            vfmadd213ps ymm2, ymm1, ymm5
            vfmadd213ps ymm2, ymm1, ymm6
            vfmadd213ps ymm2, ymm1, [k_f32_one]
            vmulps      ymm0, ymm2, ymm0
            ret
;=============================================================================
falign
fast_cosf:
;-----------------------------------------------------------------------------
            vmovaps     xmm1, xmm0
            vmulss      xmm0, xmm0, [k_f32_recip_two_pi]
            vroundss    xmm0, xmm0, xmm0, 0
            vmulss      xmm0, xmm0, [k_f32_two_pi]
            vsubss      xmm0, xmm1, xmm0
            vandps      xmm1, xmm0, [k_i32_negative_zero]
            vorps       xmm2, xmm1, [k_f32_pi]
            vandnps     xmm3, xmm1, xmm0
            vsubss      xmm4, xmm2, xmm0
            vcmpless    xmm5, xmm3, [k_f32_half_pi]
            vblendvps   xmm0, xmm4, xmm0, xmm5
            vmovaps     xmm6, [k_f32_one]
            vandnps     xmm2, xmm5, [k_f32_negative_one]
            vandps      xmm1, xmm5, xmm6
            vorps       xmm7, xmm1, xmm2
            vmulss      xmm1, xmm0, xmm0
            vmovss      xmm2, [k_f32_cos_coefficients+16]
            vfmadd213ss xmm2, xmm1, [k_f32_cos_coefficients+12]
            vfmadd213ss xmm2, xmm1, [k_f32_cos_coefficients+8]
            vfmadd213ss xmm2, xmm1, [k_f32_cos_coefficients+4]
            vfmadd213ss xmm2, xmm1, [k_f32_cos_coefficients]
            vfmadd213ss xmm2, xmm1, xmm6
            vmulss      xmm0, xmm2, xmm7
            ret
;=============================================================================
falign
fast_cos8f:
;-----------------------------------------------------------------------------
            vmovaps     ymm1, ymm0
            vmulps      ymm0, ymm0, [k_f32_recip_two_pi]
            vroundps    ymm0, ymm0, 0
            vmulps      ymm0, ymm0, [k_f32_two_pi]
            vsubps      ymm0, ymm1, ymm0
            vandps      ymm1, ymm0, [k_i32_negative_zero]
            vorps       ymm2, ymm1, [k_f32_pi]
            vandnps     ymm3, ymm1, ymm0
            vsubps      ymm4, ymm2, ymm0
            vcmpleps    ymm5, ymm3, [k_f32_half_pi]
            vblendvps   ymm0, ymm4, ymm0, ymm5
            vmovaps     ymm8, [k_f32_one]
            vandnps     ymm2, ymm5, [k_f32_negative_one]
            vandps      ymm1, ymm5, ymm8
            vorps       ymm7, ymm1, ymm2
            vmulps      ymm1, ymm0, ymm0
            vbroadcastss ymm2, [k_f32_cos_coefficients+16]
            vbroadcastss ymm3, [k_f32_cos_coefficients+12]
            vbroadcastss ymm4, [k_f32_cos_coefficients+8]
            vbroadcastss ymm5, [k_f32_cos_coefficients+4]
            vbroadcastss ymm6, [k_f32_cos_coefficients]
            vfmadd213ps ymm2, ymm1, ymm3
            vfmadd213ps ymm2, ymm1, ymm4
            vfmadd213ps ymm2, ymm1, ymm5
            vfmadd213ps ymm2, ymm1, ymm6
            vfmadd213ps ymm2, ymm1, ymm8
            vmulps      ymm0, ymm2, ymm7
            ret
;=============================================================================
falign
fast_sincosf:
;-----------------------------------------------------------------------------
            vmovaps     xmm1, xmm0
            vmulss      xmm0, xmm0, [k_f32_recip_two_pi]
            vroundss    xmm0, xmm0, xmm0, 0
            vmulss      xmm0, xmm0, [k_f32_two_pi]
            vsubss      xmm0, xmm1, xmm0
            vandps      xmm1, xmm0, [k_i32_negative_zero]
            vorps       xmm2, xmm1, [k_f32_pi]
            vandnps     xmm3, xmm1, xmm0
            vsubss      xmm4, xmm2, xmm0
            vcmpless    xmm5, xmm3, [k_f32_half_pi]
            vblendvps   xmm0, xmm4, xmm0, xmm5
            vmovaps     xmm6, [k_f32_one]
            vandnps     xmm2, xmm5, [k_f32_negative_one]
            vandps      xmm1, xmm5, xmm6
            vorps       xmm7, xmm1, xmm2
            vmulss      xmm1, xmm0, xmm0
            vmovss      xmm2, [k_f32_sin_coefficients+16]
            vmovss      xmm3, [k_f32_cos_coefficients+16]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients+12]
            vfmadd213ss xmm3, xmm1, [k_f32_cos_coefficients+12]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients+8]
            vfmadd213ss xmm3, xmm1, [k_f32_cos_coefficients+8]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients+4]
            vfmadd213ss xmm3, xmm1, [k_f32_cos_coefficients+4]
            vfmadd213ss xmm2, xmm1, [k_f32_sin_coefficients]
            vfmadd213ss xmm3, xmm1, [k_f32_cos_coefficients]
            vfmadd213ss xmm2, xmm1, xmm6
            vfmadd213ss xmm3, xmm1, xmm6
            vmulss      xmm0, xmm2, xmm0
            vmulss      xmm1, xmm3, xmm7
            ret
;=============================================================================
falign
fast_sincos8f:
;-----------------------------------------------------------------------------
            vmovaps     ymm1, ymm0
            vmulps      ymm0, ymm0, [k_f32_recip_two_pi]
            vroundps    ymm0, ymm0, 0
            vmulps      ymm0, ymm0, [k_f32_two_pi]
            vsubps      ymm0, ymm1, ymm0
            vandps      ymm1, ymm0, [k_i32_negative_zero]
            vorps       ymm2, ymm1, [k_f32_pi]
            vandnps     ymm3, ymm1, ymm0
            vsubps      ymm4, ymm2, ymm0
            vcmpleps    ymm5, ymm3, [k_f32_half_pi]
            vblendvps   ymm0, ymm4, ymm0, ymm5
            vmovaps     ymm14, [k_f32_one]
            vandnps     ymm2, ymm5, [k_f32_negative_one]
            vandps      ymm1, ymm5, ymm14
            vorps       ymm15, ymm1, ymm2
            vmulps      ymm1, ymm0, ymm0
            vbroadcastss ymm2, [k_f32_sin_coefficients+16]
            vbroadcastss ymm3, [k_f32_cos_coefficients+16]
            vbroadcastss ymm4, [k_f32_sin_coefficients+12]
            vbroadcastss ymm5, [k_f32_cos_coefficients+12]
            vbroadcastss ymm6, [k_f32_sin_coefficients+8]
            vbroadcastss ymm7, [k_f32_cos_coefficients+8]
            vbroadcastss ymm8, [k_f32_sin_coefficients+4]
            vbroadcastss ymm9, [k_f32_cos_coefficients+4]
            vbroadcastss ymm10, [k_f32_sin_coefficients]
            vbroadcastss ymm11, [k_f32_cos_coefficients]
            vfmadd213ps ymm2, ymm1, ymm4
            vfmadd213ps ymm3, ymm1, ymm5
            vfmadd213ps ymm2, ymm1, ymm6
            vfmadd213ps ymm3, ymm1, ymm7
            vfmadd213ps ymm2, ymm1, ymm8
            vfmadd213ps ymm3, ymm1, ymm9
            vfmadd213ps ymm2, ymm1, ymm10
            vfmadd213ps ymm3, ymm1, ymm11
            vfmadd213ps ymm2, ymm1, ymm14
            vfmadd213ps ymm3, ymm1, ymm14
            vmulps      ymm0, ymm2, ymm0
            vmulps      ymm1, ymm3, ymm15
            ret
;=============================================================================
; in: xmmA.xyz vector a, xmmB.xyz vector b
; out: xmmB.xyzw = dot(a, b)
; uses: xmm14 xmm15
;-----------------------------------------------------------------------------
macro vec3Dot xmmA*, xmmB* {
            vmulps      xmm14, xmmA, xmmB
            vshufps     xmmA, xmm14, xmm14, 0x00
            vshufps     xmm15, xmm14, xmm14, 0x55
            vaddps      xmmA, xmmA, xmm15
            vshufps     xmm15, xmm14, xmm14, 0xaa
            vaddps      xmmA, xmmA, xmm15 }
;=============================================================================
; in: xmmA.xyz vector a
; out: xmmA.xyzw = sqrt(dot(a, a))
; uses: xmm14 xmm15
;-----------------------------------------------------------------------------
macro vec3Length xmmA* {
            vmulps      xmm14, xmmA, xmmA
            vshufps     xmmA, xmm14, xmm14, 0x00
            vshufps     xmm15, xmm14, xmm14, 0x55
            vaddps      xmmA, xmmA, xmm15
            vshufps     xmm15, xmm14, xmm14, 0xaa
            vaddps      xmmA, xmmA, xmm15
            vsqrtps     xmmA, xmmA }
;=============================================================================
; in: xmmA.xyz vector a
; out: xmmA.xyz = normalize(a)
; uses: xmm13 xmm14 xmm15
;-----------------------------------------------------------------------------
macro vec3FastNormalize xmmA* {
            vmulps      xmm14, xmmA, xmmA
            vshufps     xmm13, xmm14, xmm14, 0x00
            vshufps     xmm15, xmm14, xmm14, 0x55
            vaddps      xmm13, xmm13, xmm15
            vshufps     xmm15, xmm14, xmm14, 0xaa
            vaddps      xmm13, xmm13, xmm15
            vrsqrtps    xmm13, xmm13
            vmulps      xmmA, xmmA, xmm13 }
;=============================================================================
; in: xmmA.xyz vector a, xmmB.xyz vector b
; out: xmmA.xyz = cross(a, b)
; uses: xmm13 xmm14 xmm15
;-----------------------------------------------------------------------------
macro vec3Cross xmmA*, xmmB* {
            vshufps     xmm14, xmmA, xmmA, 3, 1, 0, 2
            vshufps     xmm15, xmmB, xmmB, 3, 0, 2, 1
            vshufps     xmmA, xmmA, xmmA, 3, 0, 2, 1
            vshufps     xmm13, xmmB, xmmB, 3, 1, 0, 2
            vmulps      xmm14, xmm14, xmm15
            vmulps      xmmA, xmmA, xmm13
            vsubps      xmmA, xmmA, xmm14 }
;=============================================================================
; in: xmm0.xyz eye position, xmm1.xyz look at position, xmm2.xyz up vector
; out: ymm0,ymm1 "look at" matrix
falign
matrix_look_at:
;-----------------------------------------------------------------------------
            vsubps      xmm1, xmm1, xmm0                    ; eye direction
            vec3FastNormalize xmm1                          ; xmm1 = zaxis
            vec3Cross   xmm2, xmm1
            vec3FastNormalize xmm2                          ; xmm2 = xaxis
            vmovaps     xmm3, xmm1
            vec3Cross   xmm3, xmm2                          ; xmm3 = yaxis
            vxorps      xmm0, xmm0, [k_i32_negative_zero]   ; xmm0 = neg(eye position)
            vmovaps     xmm5, xmm2
            vmovaps     xmm6, xmm3
            vmovaps     xmm4, xmm1
            vec3Dot     xmm5, xmm0
            vec3Dot     xmm6, xmm0
            vec3Dot     xmm4, xmm0
            vblendps    xmm0, xmm5, xmm2, 00000111b
            vblendps    xmm2, xmm4, xmm1, 00000111b
            vblendps    xmm1, xmm6, xmm3, 00000111b
            vmovaps     xmm3, [k_f32_identity_r3]
            ; transpose
            vshufps     xmm4, xmm0, xmm1, 1, 0, 1, 0
            vshufps     xmm6, xmm0, xmm1, 3, 2, 3, 2
            vshufps     xmm5, xmm2, xmm3, 1, 0, 1, 0
            vshufps     xmm7, xmm2, xmm3, 3, 2, 3, 2
            vshufps     xmm0, xmm4, xmm5, 2, 0, 2, 0
            vshufps     xmm1, xmm4, xmm5, 3, 1, 3, 1
            vshufps     xmm2, xmm6, xmm7, 2, 0, 2, 0
            vshufps     xmm3, xmm6, xmm7, 3, 1, 3, 1
            vinsertf128 ymm0, ymm0, xmm1, 1
            vinsertf128 ymm1, ymm2, xmm3, 1
            ret
;=============================================================================
; in: xmm0.x offsetx, xmm1.x offsety, xmm2.x offsetz
; out: ymm0 = 0 0 1 0  0 0 0 1
;      ymm1 = 1 offsetz offsety offsetx  0 1 0 0
; uses: xmm15
;-----------------------------------------------------------------------------
macro matrixTranslation {
            vmovaps     xmm15, [k_f32_identity_r3]
            vinsertps   xmm15, xmm15, xmm0, 00000000b
            vinsertps   xmm15, xmm15, xmm1, 00010000b
            vinsertps   xmm15, xmm15, xmm2, 00100000b
            vmovaps     xmm2, [k_f32_identity_r2]
            vmovaps     ymm0, [k_f32_identity_r0]
            vinsertf128 ymm1, ymm2, xmm15, 1 }
;=============================================================================
; in: xmm0.x sin(angle), xmm1.x cos(angle)
; out: ymm0 = 0 sin cos 0  0 0 0 1
;      ymm1 = 1 0 0 0  0 cos -sin 0
; uses: xmm15
;-----------------------------------------------------------------------------
macro matrixRotationX {
            vxorps      xmm15, xmm15,xmm15
            vmovss      xmm0, xmm15, xmm0
            vmovss      xmm1, xmm15, xmm1
            vshufps     xmm15, xmm1, xmm0, 3, 0, 0, 3
            vmovaps     xmm0, [k_f32_identity_r0]
            vinsertf128 ymm0, ymm0, xmm15, 1
            vshufps     xmm15, xmm15, xmm15, 3, 1, 2, 0
            vxorps      xmm15, xmm15, [k_i32_negative_zero_y0]
            vmovaps     ymm1, [k_f32_identity_r2]
            vinsertf128 ymm1, ymm1, xmm15, 0 }
;=============================================================================
; in: xmm0.x sin(angle), xmm1.x cos(angle)
; out: ymm0 = 0 0 1 0  0 -sin 0 cos
;      ymm1 = 1 0 0 0  0 cos 0 sin
; uses: xmm15
;-----------------------------------------------------------------------------
macro matrixRotationY {
            vxorps      xmm15, xmm15, xmm15
            vmovss      xmm0, xmm15, xmm0
            vmovss      xmm1, xmm15, xmm1
            vshufps     xmm15, xmm0, xmm1, 3, 0, 3, 0
            vmovaps     ymm1, [k_f32_identity_r2]
            vinsertf128 ymm1, ymm1, xmm15, 0
            vshufps     xmm15, xmm15, xmm15, 3, 0, 1, 2
            vxorps      xmm15, xmm15, [k_i32_negative_zero_z0]
            vmovaps     ymm0, [k_f32_identity_r0]
            vinsertf128 ymm0, ymm0, xmm15, 0 }
;=============================================================================
; in: xmm0.x sin(angle), xmm1.x cos(angle)
; out: ymm0 = 0 0 cos -sin  0 0 sin cos
;      ymm1 = 1 0 0 0  0 1 0 0
; uses: xmm15
;-----------------------------------------------------------------------------
macro matrixRotationZ {
            vxorps      xmm15, xmm15, xmm15
            vmovss      xmm0, xmm15, xmm0
            vmovss      xmm1, xmm15, xmm1
            vunpcklps   xmm15, xmm1, xmm0
            vshufps     xmm0, xmm15, xmm15, 3, 2, 0, 1
            vxorps      xmm0, xmm0, [k_i32_negative_zero_x0]
            vinsertf128 ymm0, ymm15, xmm0, 1
            vmovaps     ymm1, [k_f32_identity_r2] }
;=============================================================================
; in: xmm0.x sin(fovy/2), xmm1.x cos(fovy/2), xmm2.x aspect, xmm3.x near z, xmm4.x far z
; out: ymm0,ymm1 perspective matrix
; uses: xmm15
;-----------------------------------------------------------------------------
macro matrixPerspectiveFov {
            vxorps      xmm15, xmm15, xmm15
            vmovss      xmm0, xmm15, xmm0
            vmovss      xmm1, xmm15, xmm1
            vdivss      xmm1, xmm1, xmm0                                ; row 1 [0 0 0 h]
            vmovaps     xmm15, [k_f32_identity_r3]                      ; xmm15 = [1 0 0 0]
            vdivss      xmm0, xmm1, xmm2                                ; row 0 [0 0 0 w]
            vinsertps   xmm1, xmm15, xmm1, 00011101b                    ; row 1 [0 0 h 0]
            vsubss      xmm2, xmm4, xmm3
            vdivss      xmm2, xmm4, xmm2                                ; row 2 [0 0 0 range]
            vmulss      xmm3, xmm2, xmm3                                ; row 3 [0 0 0 range*near_z]
            vinsertps   xmm2, xmm15, xmm2, 00100011b                    ; row 2 [1 range 0 0]
            vinsertps   xmm3, xmm15, xmm3, 00101011b
            vxorps      xmm3, xmm3, [k_i32_negative_zero_z0]            ; row 3 [0 0 -range*near_z 0]
            vinsertf128 ymm0, ymm0, xmm1, 1
            vinsertf128 ymm1, ymm2, xmm3, 1 }
;=============================================================================
; in: ymm0,ymm1 = m0; ymm2,ymm3 = m1
; out: ymm0,ymm1 = m0*m1
falign
matrix_mul:
;-----------------------------------------------------------------------------
            vperm2f128  ymm4, ymm2, ymm2, 1
            vperm2f128  ymm5, ymm3, ymm3, 1
            vmovaps     ymm12, [k_i32_perm_x0_y1]
            vmovaps     ymm13, [k_i32_perm_z0_w1]
            vmovaps     ymm14, [k_i32_perm_y0_x1]
            vmovaps     ymm15, [k_i32_perm_w0_z1]
            ; row 0, 1
            vpermps     ymm6, ymm12, ymm0
            vpermps     ymm7, ymm13, ymm0
            vpermps     ymm8, ymm14, ymm0
            vpermps     ymm9, ymm15, ymm0
            vmulps      ymm6, ymm6, ymm2
            vmulps      ymm7, ymm7, ymm3
            vmulps      ymm8, ymm8, ymm4
            vmulps      ymm9, ymm9, ymm5
            vaddps      ymm6, ymm6, ymm7
            vaddps      ymm8, ymm8, ymm9
            vaddps      ymm0, ymm6, ymm8
            ; row 2, 3
            vpermps     ymm6, ymm12, ymm1
            vpermps     ymm7, ymm13, ymm1
            vpermps     ymm8, ymm14, ymm1
            vpermps     ymm9, ymm15, ymm1
            vmulps      ymm6, ymm6, ymm2
            vmulps      ymm7, ymm7, ymm3
            vmulps      ymm8, ymm8, ymm4
            vmulps      ymm9, ymm9, ymm5
            vaddps      ymm6, ymm6, ymm7
            vaddps      ymm8, ymm8, ymm9
            vaddps      ymm1, ymm6, ymm8
            ret
;=============================================================================
; in: ymm0,ymm1 = m0; ymm2,ymm3 = m1
; out: ymm0,ymm1 = transpose(m0*m1)
falign
matrix_mul_transpose:
;-----------------------------------------------------------------------------
            vperm2f128  ymm4, ymm2, ymm2, 1
            vperm2f128  ymm5, ymm3, ymm3, 1
            vmovaps     ymm12, [k_i32_perm_x0_y1]
            vmovaps     ymm13, [k_i32_perm_z0_w1]
            vmovaps     ymm14, [k_i32_perm_y0_x1]
            vmovaps     ymm15, [k_i32_perm_w0_z1]
            ; row 0, 1
            vpermps     ymm6, ymm12, ymm0
            vpermps     ymm7, ymm13, ymm0
            vpermps     ymm8, ymm14, ymm0
            vpermps     ymm9, ymm15, ymm0
            vmulps      ymm6, ymm6, ymm2
            vmulps      ymm7, ymm7, ymm3
            vmulps      ymm8, ymm8, ymm4
            vmulps      ymm9, ymm9, ymm5
            vaddps      ymm6, ymm6, ymm7
            vaddps      ymm8, ymm8, ymm9
            vaddps      ymm10, ymm6, ymm8
            ; row 2, 3
            vpermps     ymm6, ymm12, ymm1
            vpermps     ymm7, ymm13, ymm1
            vpermps     ymm8, ymm14, ymm1
            vpermps     ymm9, ymm15, ymm1
            vmulps      ymm6, ymm6, ymm2
            vmulps      ymm7, ymm7, ymm3
            vmulps      ymm8, ymm8, ymm4
            vmulps      ymm9, ymm9, ymm5
            vaddps      ymm6, ymm6, ymm7
            vaddps      ymm8, ymm8, ymm9
            vaddps      ymm11, ymm6, ymm8
            ; transpose
            vunpcklps   ymm4, ymm10, ymm11
            vunpckhps   ymm5, ymm10, ymm11
            vperm2f128  ymm10, ymm4, ymm5, 0x20
            vperm2f128  ymm11, ymm4, ymm5, 0x31
            vunpcklps   ymm4, ymm10, ymm11
            vunpckhps   ymm5, ymm10, ymm11
            vperm2f128  ymm0, ymm4, ymm5, 0x20
            vperm2f128  ymm1, ymm4, ymm5, 0x31
            ret
;=============================================================================

