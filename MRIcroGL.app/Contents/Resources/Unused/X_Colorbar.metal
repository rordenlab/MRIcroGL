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

// Fragment shader
fragment float4 fragmentShader(VertexOut in [[stage_in]]) {
	return in.color;
	//return float4(in.color.xyz, 0.1);
	//return float4(1.0, 0.0, 0.0, 1.0);
   	//float4 clr = pngTexture.sample(textureSampler, in.color.xyz);
   	//return float4(mix(fragUniforms->clearClr.rgb, clr.rgb, clr.a), 1.0);
}

