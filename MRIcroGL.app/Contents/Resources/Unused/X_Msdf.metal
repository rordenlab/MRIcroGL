#include <metal_stdlib>

using namespace metal;


typedef struct {
    float2 position; //vertex x,y position in pixel space
    float4 color; //vertex color
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
             constant VertexIn *vertices [[buffer(0)]],
             const device Uniforms* uniforms [[ buffer(1) ]]){
    float2 pixelPosition = vertices[vertexID].position.xy;
	float2 viewportSize = uniforms->viewportSize;
    VertexOut out;
    pixelPosition -= (viewportSize/2.0);
    out.position = vector_float4((pixelPosition / (viewportSize/2)), 0.0, 1.0);
    out.color = vertices[vertexID].color;
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
   	//return float4(1.0, 0.0, 0.0, opacity);
   	return float4(fragUniforms->fontClr.rgb, opacity); //correct solution with renderbufferAttachment.blendingEnabled = YES
}

