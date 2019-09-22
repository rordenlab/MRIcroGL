//pref
//unused1|float|0.0|1.0|1
//unused2|float|0.0|0.0|1
//unused3|float|0.0|0.0|1
//unused4|float|0.01|10.0|30
//overAlpha|float|0.0|0.8|2.0
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
	float overAlpha;
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
               texture3d<float> overlayVolTexture [[ texture(2) ]],
               texture3d<float> overlayGradTexture [[ texture(3) ]],
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
	float clipStart = 0.0;
	float clipEnd = len;
	float stepSizeX2 = -stepSize;

	if (clipPlane.a > -0.5) {
		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);
		float dis = dot(dir,clipPlane.xyz);
		if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;
		//test: "return" fails on 2006MacBookPro10.4ATI1900, "discard" fails on MacPro10.5NV8800
		//if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0)))
		// return float4(0.0,0.0,0.0,0.0);
		//if ((dis > 0.0) && (dis < len)) {
		//	if (frontface)
		//		start = start + dir * dis;
		//	else
		//		backPosition =  start + dir * (dis);
		//	len = length(backPosition - start);
		//}
		if (frontface) {
			clipStart = dis;
			stepSizeX2 = clipStart + (stepSize * 2); //avoid specular effects in clip plane
		} else
			clipEnd =  dis;
	}
	float3 lightPosition = fragUniforms->lightPos.xyz;
	//only for overlays: ambient,diffuse,specular,shininess
	float overAlpha = customFragUniforms->overAlpha;
	float ambient = 1.0;
	float diffuse = 0.3;
	float specular = 0.25;
	float shininess = 10.0;
	float3 deltaDir = dir * stepSize;
	float4 gradSample, colorSample,colAcc = float4(0.0,0.0,0.0,0.0);
	float4 ocolAcc = float4(0.0,0.0,0.0,0.0);
	float4 oprevGrad = float4(0.0,0.0,0.0,0.0);
	float lengthAcc = 0.0;
	float3 samplePos = start.xyz;
	float2 gl_FragCoord = float2(in.position.xy); //random jitter to reduce wood grain
	//stochastic jitter http://www.mccauslandcenter.sc.edu/mricrogl/shaders
	samplePos += deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));
	while (lengthAcc <= len) {
		if ((lengthAcc <= clipStart) || (lengthAcc > clipEnd))
			colorSample = float4(0.0,0.0,0.0,0.0);
		else
		#ifdef GRADIENT
			colorSample = (gradTexture.sample(textureSampler, samplePos));
		#else
			colorSample = (volTexture.sample(textureSampler, samplePos));
		#endif
		if (colorSample.a > colAcc.a)
			colAcc = colorSample;

		colorSample.rgba = (overlayVolTexture.sample(textureSampler, samplePos));
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
		if (colorSample.a > 0.01) {
			gradSample = (overlayGradTexture.sample(textureSampler, samplePos)).rgba;
			gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
			if (gradSample.a < oprevGrad.a)
				gradSample.rgb = oprevGrad.rgb;
			oprevGrad = gradSample;
			float lightNormDot = dot(gradSample.rgb, lightPosition);

			float3 a = colorSample.rgb * ambient;
			float3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
			float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
			colorSample.rgb = a + d + s;
			colorSample.rgb *= colorSample.a;
			ocolAcc = (1.0 - ocolAcc.a) * colorSample + ocolAcc;
		}
		samplePos += deltaDir;
		lengthAcc += stepSize;
		//if ( lengthAcc >= len || colAcc.a > 0.95 )
		if ( lengthAcc >= len)
			break;
	}
	if (ocolAcc.a > 0.01)  {
		//ocolAcc.rgb *= 1/ocolAcc.a;
		ocolAcc.a *= overAlpha;
		colAcc.a=max(colAcc.a,ocolAcc.a);
		//colAcc = float4(overAlpha, 0.0, 0.0, overAlpha);
		colAcc.rgb=mix(colAcc.rgb, ocolAcc.rgb, ocolAcc.a);
	}
	if (ocolAcc.a > 2.01)  {
		//float oa = customFragUniforms->overAlpha;
		ocolAcc.a = overAlpha;
		ocolAcc.rgb *= 1/ocolAcc.a;
		//colAcc.rgb *= 1/colAcc.a;
		float frac1 = colAcc.a/(colAcc.a +ocolAcc.a);
		colAcc.a=max(colAcc.a,ocolAcc.a);

		//colAcc.rgb = ocolAcc.rgb;
		//colAcc.rgb=mix(colAcc.rgb, ocolAcc.rgb,  ocolAcc.a * customFragUniforms->overAlpha);
		colAcc.rgb=mix(colAcc.rgb, ocolAcc.rgb, 1.0- frac1);
	}
	//colAcc = ocolAcc;
	//colAcc.a = colAcc.a/0.95;
    return float4(colAcc);
}
