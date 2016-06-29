#define rs \
    "RootFlags(ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT), " \
    "CBV(b0, visibility = SHADER_VISIBILITY_VERTEX)"

struct transform_t
{
  float4x4 mat;
};
ConstantBuffer<transform_t> g_transform : register(b0);

// VS
[RootSignature(rs)]
float4 object_vs(float4 position : POSITION) : SV_Position
{
  return mul(float4(position.xyz, 1.0f), g_transform.mat);
}

// PS
[RootSignature(rs)]
float4 object_ps(float4 position : SV_Position) : SV_Target0
{
  return float4(1.0f, 0.9f, 0.0f, 1.0f);
}


