#include <metal_stdlib>
//Signed Distance Field Font
//  xcrun -sdk macosx metal -c msdf.metal -o msdf.air
//  xcrun -sdk macosx metallib msdf.air -o msdf.metallib

using namespace metal;


typedef struct {
    float4 position; //vertex x,y position in pixel space, z,w = x,y in texture space
} VertexIn;

typedef struct {
    float4 position [[position]];
    float4 color;
} VertexOut;

struct Uniforms {
	float2 viewportSize;
};

struct FragUniforms {
	float4 fontClr;
};

// Vertex shader
vertex VertexOut vertexShader(uint vertexID [[vertex_id]],
             const device VertexIn* vertices    [[ buffer(0) ]],
             const device Uniforms* uniforms [[ buffer(1) ]]){
    float2 pixelPosition = vertices[vertexID].position.xy;
    float2 textureUV = vertices[vertexID].position.zw;
	float2 viewportSize = uniforms->viewportSize;
    VertexOut out;
    pixelPosition -= (viewportSize/2.0);
    out.position = vector_float4((pixelPosition / (viewportSize/2)), 0.0, 1.0);
    out.color = float4(textureUV, 0.0, 1.0);
    return out;
}

float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
}

// Fragment shader
fragment float4 fragmentShader(VertexOut in [[stage_in]],
texture3d<float> pngTexture [[ texture(0) ]],
const device FragUniforms* fragUniforms    	[[ buffer(1) ]]
 ) {
    constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
   	float3 sample = pngTexture.sample(textureSampler, in.color.xyz).rgb;
    float sigDist = median(sample.r, sample.g, sample.b) - 0.5;
   	float opacity = clamp(sigDist/fwidth(sigDist) + 0.5, 0.0, 1.0);
   	//return float4(0.0, 0.5, 0.0, opacity);
   	return float4(fragUniforms->fontClr.rgb, opacity); //correct solution with renderbufferAttachment.blendingEnabled = YES
}

