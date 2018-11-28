#include <metal_stdlib>

using namespace metal;

typedef struct {
    float2 position; //vertex x,y position in pixel space
    float4 color; //vertex color: position in 3D texture
} VertexIn;

typedef struct {
    float4 position [[position]];
    float4 color;
} VertexOut;

struct Uniforms {
	float2 viewportSize;
};

struct FragUniforms {
	float backAlpha, pad0,pad1,pad2;
};
// Vertex shader
vertex VertexOut vertexShader(uint vertexID [[vertex_id]],
             constant VertexIn *vertices [[buffer(0)]],
             const device Uniforms* uniforms [[ buffer(1) ]]){
    float2 pixelPosition = vertices[vertexID].position.xy;
	float2 viewportSize = uniforms->viewportSize;
	pixelPosition -= (viewportSize/2.0);
    VertexOut out;
    out.position = vector_float4((pixelPosition / (viewportSize/2)), 0.0, 1.0);
    out.color = vertices[vertexID].color;
    return out;
}

// Fragment shader
fragment float4 fragmentShader(VertexOut in [[stage_in]],
const device FragUniforms* fragUniforms [[ buffer(2) ]],
texture3d<float> volTexture [[ texture(0) ]],
texture3d<float> overlayVolTexture [[ texture(1) ]]
 ) {
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
	float4 color = volTexture.sample(textureSampler, in.color.xyz);
	if (color.a > 0.0)
		color.a = fragUniforms->backAlpha;
	float4 ocolor = overlayVolTexture.sample(textureSampler, in.color.xyz);
	color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);
	color.a = max(color.a, ocolor.a);
   	return color;
}