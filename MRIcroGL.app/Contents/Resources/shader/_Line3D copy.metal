//vert
#include <metal_stdlib>
//xcrun -sdk macosx metal -c _Line3D.metal -o Render.air 

using namespace metal;

struct VertexIn {
	float3 position;
	float4 color;
};

struct VertexOut {
	float4 position [[position]];
	float4 color;
};

struct Uniforms {
	float4x4 modelViewProjectionMatrix;
};

vertex VertexOut vertexShader(  unsigned int vertexID [[ vertex_id ]],
								const device VertexIn* verts [[ buffer(0) ]],
								const device Uniforms* uniforms [[ buffer(1) ]]
							) {
	VertexIn VertexIn = verts[vertexID];
	VertexOut VertexOut;
	VertexOut.position = uniforms->modelViewProjectionMatrix * float4(VertexIn.position, 1);
	VertexOut.color = VertexIn.color;
	return VertexOut;
}

fragment float4 fragmentShader(VertexOut  in [[stage_in]]) {
	return in.color;
}
