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
fragment float4 fragmentShader(VertexOut in [[stage_in]]) {
	float4 color = in.color;
	//color.g = 1.0;
	//color.a = 1.0;
	//if (color.a > 0.) color.a = 1.0;
   	return color;
}