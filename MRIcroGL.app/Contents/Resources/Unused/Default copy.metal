//pref
//ambient|float|0.0|1.0|1
//diffuse|float|0.0|0.3|1
//specular|float|0.0|0.25|1
//shininess|float|0.01|10.0|30
//vert
#include <metal_stdlib>
//xcrun -sdk macosx metal -c Default.metal -o Render.air
//#define FRONTFACE
//#define BACKFACE
//#define GRADIENT
//#define NO_LIGHT

using namespace metal;

struct CustomFragUniforms {
	float ambient;
	float diffuse;
	float specular;
	float shininess;
};

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

struct FragUniforms {
	float stepSiz;
	float sliceSiz;
	float x0;
	float x1;
	float4 rayDir;
	float4 lightPos;
	float4 clipPlane;
};

vertex VertexOut vertexShader(  unsigned int vertexID               [[ vertex_id ]],
                                const device VertexIn* verts    [[ buffer(0) ]],
								const device Uniforms* uniforms    	[[ buffer(1) ]]
                                ) {
	VertexIn VertexIn = verts[vertexID];
	VertexOut VertexOut;
	VertexOut.position = uniforms->modelViewProjectionMatrix * float4(VertexIn.position, 1);
	VertexOut.color = VertexIn.color;
	return VertexOut;
}

float3 GetBackPosition (float3 startPosition, float3 rayDir) {
	//assume orthographic projection - perspective a bit trickier
	// http://prideout.net/blog/?p=64
	float3 invR = 1.0 / (rayDir);
    float3 tbot = invR * (float3(0.0)-startPosition);
    float3 ttop = invR * (float3(1.0)-startPosition);
    float3 tmax = max(ttop, tbot);
    float2 t = min(tmax.xx, tmax.yz);
	return startPosition + (rayDir * min(t.x, t.y));
}

fragment float4 fragmentShader(VertexOut  in [[stage_in]],
               texture3d<float> volTexture [[ texture(0) ]],
               texture3d<float> gradTexture [[ texture(1) ]],
               const device FragUniforms* fragUniforms    	[[ buffer(1) ]],
               const device CustomFragUniforms* customFragUniforms    	[[ buffer(2) ]]
               ) {
	//return float4(1,0,1,1);
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
    float3 start = in.color.rgb;
    #ifdef FRONTFACE
    	return float4(start,1);//<- show front face
    #endif
	float3 backPosition = GetBackPosition(start, fragUniforms->rayDir.xyz);
	#ifdef BACKFACE
		return float4(backPosition,1); //<- show back face
	#endif
	float sliceSize = fragUniforms->sliceSiz;//for opacity correction
	float stepSize = fragUniforms->stepSiz;//sampling rate
	float3 dir = backPosition - start;
	float4 clipPlane = fragUniforms->clipPlane;
	float len = length(dir);
	dir = normalize(dir);
	if (clipPlane.a > -0.5) {
		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);
		float dis = dot(dir,clipPlane.xyz);
		if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;
		//test: "return" fails on 2006MacBookPro10.4ATI1900, "discard" fails on MacPro10.5NV8800
		if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0)))
		 return float4(0.0,0.0,0.0,0.0);
		if ((dis > 0.0) && (dis < len)) {
			if (frontface)
				start = start + dir * dis;
			else
				backPosition =  start + dir * (dis);
			len = length(backPosition - start);
		}
	}
	float3 lightPosition = fragUniforms->lightPos.xyz;
	float ambient = customFragUniforms->ambient;
	float diffuse = customFragUniforms->diffuse;
	float specular = customFragUniforms->specular;
	float shininess = customFragUniforms->shininess;
	float3 deltaDir = dir * stepSize;
	float4 gradSample, colorSample,colAcc = float4(0.0,0.0,0.0,0.0);
	float4 prevGrad = float4(0.0,0.0,0.0,0.0);
	float lengthAcc = 0.0;
	float3 samplePos = start.xyz;
	float2 gl_FragCoord = float2(in.position.xy); //random jitter to reduce wood grain
	//stochastic jitter http://www.mccauslandcenter.sc.edu/mricrogl/shaders
	samplePos += deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));
	float stepSizeX2 = stepSize * 3; //avoid specular effects in clip plane
	while (lengthAcc <= len) {
		#ifdef GRADIENT
			colorSample = (gradTexture.sample(textureSampler, samplePos));
		#else
			colorSample = (volTexture.sample(textureSampler, samplePos));
		#endif
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
		if ((colorSample.a > 0.01) && (lengthAcc > stepSizeX2)) {
			//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
			gradSample = (gradTexture.sample(textureSampler, samplePos)).rgba;
			gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
			//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
			if (gradSample.a < prevGrad.a)
				gradSample.rgb = prevGrad.rgb;
			prevGrad = gradSample;
			float lightNormDot = dot(gradSample.rgb, lightPosition);
			float3 a = colorSample.rgb * ambient;
			float3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
			float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
			colorSample.rgb = a + d + s;
		}
		colorSample.rgb *= colorSample.a;
		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
		samplePos += deltaDir;
		lengthAcc += stepSize;
		if ( lengthAcc >= len || colAcc.a > 0.95 )
			break;
	}
	colAcc.a = colAcc.a/0.95;
    return float4(colAcc);
}
