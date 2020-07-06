#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;

typedef enum AAPLTextureIndex
{
    AAPLTextureIndexInput  = 0,
    AAPLTextureIndexOutput = 1,
} AAPLTextureIndex;

kernel void
sobelKernel(texture3d<half>  inTexture  [[texture(AAPLTextureIndexInput)]],
                texture3d<half, access::write> outTexture [[texture(AAPLTextureIndexOutput)]],
                uint3                          gid         [[thread_position_in_grid]])
{
    if((gid.x >= outTexture.get_width()) || (gid.y >= outTexture.get_height()) || (gid.z >= outTexture.get_depth()))
    {
        // Return early if the pixel is out of bounds
        return;
    }
    float dX = 1.0/(outTexture.get_width() - 1.0); //voxel width as fraction of texture width
    float dY = 1.0/(outTexture.get_height() - 1.0); //voxel height as fraction of texture height
	float dZ = 1.0/(outTexture.get_depth() - 1.0); //voxel slice as fraction of texture slices
	float3 vx = float3(gid.xyz) * float3(dX,dY,dZ); //pixel to fractional coordinates
	float blurVox = 1.2;
	dX *= blurVox;
	dY *= blurVox;
	dZ *= blurVox;
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
	float TAR = inTexture.sample(textureSampler, vx+float3(+dX,+dY,+dZ)).a;
	float TAL = inTexture.sample(textureSampler, vx+float3(+dX,+dY,-dZ)).a;
	float TPR = inTexture.sample(textureSampler, vx+float3(+dX,-dY,+dZ)).a;
	float TPL = inTexture.sample(textureSampler, vx+float3(+dX,-dY,-dZ)).a;
	float BAR = inTexture.sample(textureSampler, vx+float3(-dX,+dY,+dZ)).a;
	float BAL = inTexture.sample(textureSampler, vx+float3(-dX,+dY,-dZ)).a;
	float BPR = inTexture.sample(textureSampler, vx+float3(-dX,-dY,+dZ)).a;
	float BPL = inTexture.sample(textureSampler, vx+float3(-dX,-dY,-dZ)).a;
	float4 gradientSample;
	gradientSample.r =   BAR+BAL+BPR+BPL -TAR-TAL-TPR-TPL;
	gradientSample.g =  TPR+TPL+BPR+BPL -TAR-TAL-BAR-BAL;
	gradientSample.b =  TAL+TPL+BAL+BPL -TAR-TPR-BAR-BPR;
	gradientSample.a = (abs(gradientSample.r)+abs(gradientSample.g)+abs(gradientSample.b))*0.29;
	gradientSample.rgb = normalize(gradientSample.rgb);
	gradientSample.rgb =  (gradientSample.rgb * 0.5)+0.5;
    outTexture.write(half4(gradientSample), gid);
}

kernel void
blurKernel(texture3d<half>  inTexture  [[texture(AAPLTextureIndexInput)]],
                texture3d<half, access::write> outTexture [[texture(AAPLTextureIndexOutput)]],
                uint3                          gid         [[thread_position_in_grid]])
{
    if((gid.x >= outTexture.get_width()) || (gid.y >= outTexture.get_height()) || (gid.z >= outTexture.get_depth()))
    {
        // Return early if the pixel is out of bounds
        return;
    }
    float dX = 1.0/(outTexture.get_width() - 1.0); //voxel width as fraction of texture width
    float dY = 1.0/(outTexture.get_height() - 1.0); //voxel height as fraction of texture height
	float dZ = 1.0/(outTexture.get_depth() - 1.0); //voxel slice as fraction of texture slices
	float3 vx = float3(gid.xyz) * float3(dX,dY,dZ); //pixel to fractional coordinates
	float blurVox = 0.7;
	dX *= blurVox;
	dY *= blurVox;
	dZ *= blurVox;
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
	float alpha;
	alpha  = inTexture.sample(textureSampler,vx+float3(+dX,+dY,+dZ)).a;
	alpha += inTexture.sample(textureSampler,vx+float3(+dX,-dY,+dZ)).a;
	alpha += inTexture.sample(textureSampler,vx+float3(-dX,+dY,+dZ)).a;
	alpha += inTexture.sample(textureSampler,vx+float3(-dX,-dY,+dZ)).a;
	//below
	alpha += inTexture.sample(textureSampler,vx+float3(+dX,+dY,-dZ)).a;
	alpha += inTexture.sample(textureSampler,vx+float3(+dX,-dY,-dZ)).a;
	alpha += inTexture.sample(textureSampler,vx+float3(-dX,+dY,-dZ)).a;
	alpha += inTexture.sample(textureSampler,vx+float3(-dX,-dY,-dZ)).a;
	alpha *= 0.125;
	outTexture.write(half4(alpha, alpha, alpha, alpha), gid);
}