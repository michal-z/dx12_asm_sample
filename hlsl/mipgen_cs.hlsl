#define rs_mipgen \
  "RootConstants(b0, num32BitConstants = 2), " \
  "DescriptorTable(SRV(t0), UAV(u0, numDescriptors = 4)), " \

Texture2D<float4> srv_src_mip : register(t0);
RWTexture2D<float4> uav_out_mip0 : register(u0);
RWTexture2D<float4> uav_out_mip1 : register(u1);
RWTexture2D<float4> uav_out_mip2 : register(u2);
RWTexture2D<float4> uav_out_mip3 : register(u3);

struct params_t {
  uint src_mip_level;
  uint num_mip_levels;
};
ConstantBuffer<params_t> cbv_params : register(b0);

groupshared float g_red[64];
groupshared float g_green[64];
groupshared float g_blue[64];
groupshared float g_alpha[64];

void store_color(uint idx, float4 color) {
  g_red[idx] = color.r;
  g_green[idx] = color.g;
  g_blue[idx] = color.b;
  g_alpha[idx] = color.a;
}

float4 load_color(uint idx) {
  return float4(g_red[idx], g_green[idx], g_blue[idx], g_alpha[idx]);
}

[RootSignature(rs_mipgen)]
[numthreads(8, 8, 1)]
void mipgen_cs(uint3 global_idx : SV_DispatchThreadID, uint group_idx : SV_GroupIndex) {
  uint x = global_idx.x * 2;
  uint y = global_idx.y * 2;

  float4 s00 = srv_src_mip.mips[cbv_params.src_mip_level][uint2(x, y)];
  float4 s10 = srv_src_mip.mips[cbv_params.src_mip_level][uint2(x + 1, y)];
  float4 s01 = srv_src_mip.mips[cbv_params.src_mip_level][uint2(x, y + 1)];
  float4 s11 = srv_src_mip.mips[cbv_params.src_mip_level][uint2(x + 1, y + 1)];
  s00 = 0.25f * (s00 + s01 + s10 + s11);

  uav_out_mip0[global_idx.xy] = s00;
  store_color(group_idx, s00);

  if (cbv_params.num_mip_levels == 1) return;
  GroupMemoryBarrierWithGroupSync();

  if ((group_idx & 0x9) == 0) {
    s10 = load_color(group_idx + 1);
    s01 = load_color(group_idx + 8);
    s11 = load_color(group_idx + 9);
    s00 = 0.25f * (s00 + s01 + s10 + s11);

    uav_out_mip1[global_idx.xy / 2] = s00;
    store_color(group_idx, s00);
  }

  if (cbv_params.num_mip_levels == 2) return;
  GroupMemoryBarrierWithGroupSync();

  if ((group_idx & 0x1b) == 0) {
    s10 = load_color(group_idx + 2);
    s01 = load_color(group_idx + 16);
    s11 = load_color(group_idx + 18);
    s00 = 0.25f * (s00 + s01 + s10 + s11);

    uav_out_mip2[global_idx.xy / 4] = s00;
    store_color(group_idx, s00);
  }

  if (cbv_params.num_mip_levels == 3) return;
  GroupMemoryBarrierWithGroupSync();

  if (group_idx == 0) {
    s10 = load_color(group_idx + 4);
    s01 = load_color(group_idx + 32);
    s11 = load_color(group_idx + 36);
    s00 = 0.25f * (s00 + s01 + s10 + s11);

    uav_out_mip3[global_idx.xy / 8] = s00;
    store_color(group_idx, s00);
  }
}
