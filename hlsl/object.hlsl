#define rs \
  "RootFlags(ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT), " \
  "CBV(b0, visibility = SHADER_VISIBILITY_VERTEX), " \
  "DescriptorTable(SRV(t0), visibility = SHADER_VISIBILITY_PIXEL), " \
  "StaticSampler(s0, filter = FILTER_ANISOTROPIC, visibility = SHADER_VISIBILITY_PIXEL)"

struct vs_in {
  float2 position : POSITION;
  float2 texcoord : TEXCOORD;
};

struct vs_out {
  float4 position : SV_Position;
  float2 texcoord : TEXCOORD;
};

struct transform_t {
  float4x4 mat;
};
ConstantBuffer<transform_t> cbv_transform : register(b0);


//; VS
[RootSignature(rs)]
vs_out object_vs(vs_in i) {
  vs_out o;
  o.position = mul(float4(i.position, 0.0f, 1.0f), cbv_transform.mat);
  o.texcoord = i.texcoord;
  return o;
}


//; PS
Texture2D<float4> srv_texture : register(t0);
SamplerState s_sampler : register(s0);

[RootSignature(rs)]
float4 object_ps(vs_out i) : SV_Target0 {
  return srv_texture.Sample(s_sampler, i.texcoord);
}


