#include <metal_stdlib>

using namespace metal;

typedef struct {
	float3 position;
	float4 color;
} VertexIn;


typedef struct {
    float4 position [[position]];
    float4 color;
} VertexOut;

struct Uniforms {
	float4x4 modelViewProjectionMatrix;
};

// Vertex shader
vertex VertexOut vertexShader(uint vertexID [[vertex_id]],
             constant VertexIn *vertices [[buffer(0)]],
             const device Uniforms* uniforms [[ buffer(1) ]]){

	VertexIn VertexIn = vertices[vertexID];
	VertexOut VertexOut;
	VertexOut.position = uniforms->modelViewProjectionMatrix * float4(VertexIn.position, 1);
	VertexOut.color = VertexIn.color;
	return VertexOut;
}

// Fragment shader
fragment float4 fragmentShader(VertexOut in [[stage_in]]) {
	return in.color;
}

