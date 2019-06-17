#include <metal_stdlib>
//xcrun -sdk macosx metal -c _Texture2Dx.metal -o Render.air

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
	float backAlpha, drawAlpha,pad1,pad2;
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
texture3d<float> overlayVolTexture [[ texture(1) ]],
texture3d<float> drawVolTexture [[ texture(2) ]],
texture1d<float> drawVolLUT [[ texture(3) ]]
 ) {
	constexpr sampler texSampL (mag_filter::linear,min_filter::linear);
	constexpr sampler texSampN (mag_filter::nearest,min_filter::nearest);

	float4 color = volTexture.sample(texSampN, in.color.xyz);
	//float4 color = volTexture.sample(texSampL, in.color.xyz);
	if (color.a > 0.0)
		color.a = fragUniforms->backAlpha;
	float4 ocolor = overlayVolTexture.sample(texSampN, in.color.xyz);
	//float4 ocolor = overlayVolTexture.sample(texSampL, in.color.xyz);
	color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);
	color.a = max(color.a, ocolor.a);
   	if (fragUniforms->drawAlpha == 0.0) return color;
	//ocolor = drawVolTexture.sample(texSampN, in.color.xyz);
	ocolor = drawVolLUT.sample(texSampN, drawVolTexture.sample(texSampN, in.color.xyz).r);
	ocolor.a *= fragUniforms->drawAlpha;
	color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);
	color.a = max(color.a, ocolor.a);
   	return color;

}