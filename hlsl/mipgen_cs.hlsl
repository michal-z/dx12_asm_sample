#define RsMipmap \
    "RootConstants(b0, num32BitConstants = 2), " \
    "DescriptorTable(SRV(t0), UAV(u0, numDescriptors = 4)), " \

Texture2D<float4> gSrcMip : register(t0);
RWTexture2D<float4> gOutMip1 : register(u0);
RWTexture2D<float4> gOutMip2 : register(u1);
RWTexture2D<float4> gOutMip3 : register(u2);
RWTexture2D<float4> gOutMip4 : register(u3);
SamplerState gSampler : register(s0);

struct Params
{
    uint SrcMipLevel;
    uint NumMipLevels;
};
ConstantBuffer<Params> gParams : register(b0);

groupshared float gRed[64];
groupshared float gGreen[64];
groupshared float gBlue[64];
groupshared float gAlpha[64];

void StoreColor(uint idx, float4 color)
{
    gRed[idx] = color.r;
    gGreen[idx] = color.g;
    gBlue[idx] = color.b;
    gAlpha[idx] = color.a;
}

float4 LoadColor(uint idx)
{
    return float4(gRed[idx], gGreen[idx], gBlue[idx], gAlpha[idx]);
}

[RootSignature(RsMipmap)]
[numthreads(8, 8, 1)]
void mipgen_cs(uint3 globalIdx : SV_DispatchThreadID, uint groupIdx : SV_GroupIndex)
{
    uint x = globalIdx.x * 2;
    uint y = globalIdx.y * 2;

    float4 s00 = gSrcMip.mips[gParams.SrcMipLevel][uint2(x, y)];
    float4 s10 = gSrcMip.mips[gParams.SrcMipLevel][uint2(x + 1, y)];
    float4 s01 = gSrcMip.mips[gParams.SrcMipLevel][uint2(x, y + 1)];
    float4 s11 = gSrcMip.mips[gParams.SrcMipLevel][uint2(x + 1, y + 1)];
    s00 = 0.25f * (s00 + s01 + s10 + s11);

    gOutMip1[globalIdx.xy] = s00;
    StoreColor(groupIdx, s00);

    if (gParams.NumMipLevels == 1) return;
    GroupMemoryBarrierWithGroupSync();

    if ((groupIdx & 0x9) == 0)
    {
        s10 = LoadColor(groupIdx + 1);
        s01 = LoadColor(groupIdx + 8);
        s11 = LoadColor(groupIdx + 9);
        s00 = 0.25f * (s00 + s01 + s10 + s11);

        gOutMip2[globalIdx.xy / 2] = s00;
        StoreColor(groupIdx, s00);
    }

    if (gParams.NumMipLevels == 2) return;
    GroupMemoryBarrierWithGroupSync();

    if ((groupIdx & 0x1b) == 0)
    {
        s10 = LoadColor(groupIdx + 2);
        s01 = LoadColor(groupIdx + 16);
        s11 = LoadColor(groupIdx + 18);
        s00 = 0.25f * (s00 + s01 + s10 + s11);

        gOutMip3[globalIdx.xy / 4] = s00;
        StoreColor(groupIdx, s00);
    }

    if (gParams.NumMipLevels == 3) return;
    GroupMemoryBarrierWithGroupSync();

    if (groupIdx == 0)
    {
        s10 = LoadColor(groupIdx + 4);
        s01 = LoadColor(groupIdx + 32);
        s11 = LoadColor(groupIdx + 36);
        s00 = 0.25f * (s00 + s01 + s10 + s11);

        gOutMip4[globalIdx.xy / 8] = s00;
        StoreColor(groupIdx, s00);
    }
}

// vim: cindent ft= :
