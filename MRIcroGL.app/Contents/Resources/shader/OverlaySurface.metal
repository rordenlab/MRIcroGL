//pref
//ambient|float|0.0|1.0|1|Illuminate surface regardless of lighting
//diffuse|float|0.0|0.2|1|Illuminate surface based on light position
//specular|float|0.0|0.2|1|Glint from shiny surfaces
//shininess|float|0.01|10.0|30|Specular reflections can be rough or precise
//overlayOpacity|float|0.0|0.8|1.0|Translucency of overlays
//overlayDepth|float|0.0|8.0|64.0|Search depth to detect overlays

//vert
#include <metal_stdlib>
//xcrun -sdk macosx metal -c MX.metal -o Render.air

using namespace metal;

struct CustomFragUniforms {
	float ambient, diffuse, specular, shininess, overlayOpacity, overlayDepth;

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
	float overlayNum;
	float backAlpha;
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
    float3 backPosition = GetBackPosition(start, fragUniforms->rayDir.xyz);
	float sliceSize = fragUniforms->sliceSiz;//for opacity correction
	float stepSize = fragUniforms->stepSiz;//sampling rate
	float3 dir = backPosition - start;
	float4 clipPlane = fragUniforms->clipPlane;
	float len = length(dir);
	dir = normalize(dir);
	float3 lightPosition = fragUniforms->lightPos.xyz;
	float ambient = customFragUniforms->ambient;
	float diffuse = customFragUniforms->diffuse;
	float specular = customFragUniforms->specular;
	float shininess = customFragUniforms->shininess;
	float3 deltaDir = dir * stepSize;
	float4 gradSample, colorSample;
	float2 gl_FragCoord = float2(in.position.xy); //random jitter to reduce wood grain
	float stepSizeX2 = stepSize * 2.0; //avoid specular effects in clip plane

	float4 overSample = float4(0.0,0.0,0.0,0.0);
	float4 colAcc = float4(0.0,0.0,0.0,0.0);
	float4 prevGrad = float4(0.0,0.0,0.0,0.0);
	float lengthAcc = 0.0;
	float3 samplePos;
	samplePos = start.xyz +deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));
	if (clipPlane.a > -0.5) {
		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);
		float dis = dot(dir,clipPlane.xyz);
		if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;
		//test: "return" fails on 2006MacBookPro10.4ATI1900, "discard" fails on MacPro10.5NV8800
		if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0)))
			lengthAcc = len + 1.0; //no background
		else if ((dis > 0.0) && (dis < len)) {
			if (frontface) {
				lengthAcc = dis;
				//stepSizeX2 = dis;
				samplePos += dir * dis;
				//len -= dir * dis;
			} else {
				backPosition =  start + dir * (dis);
				len = length(backPosition - start);
			}
		}
	}
	stepSizeX2 = lengthAcc + (stepSize * 2.0);
	float isSurface = 0.0;
	if (fragUniforms->overlayNum < 1.0) isSurface = 1.0;

	while (lengthAcc <= len) {
		colorSample = (volTexture.sample(textureSampler, samplePos));
		if (colorSample.a > 0.0) {
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
			//start overlay()
			if (isSurface < 1.0) {
				float overlayLengthAcc = 0.0;
				float3 overPos = samplePos;
				float overlaySearchDepth = customFragUniforms->overlayDepth * sliceSize;
				float maxA = 0.0;
				while (overlayLengthAcc <= overlaySearchDepth) {
					float4 overSam = overlayVolTexture.sample(textureSampler, overPos);
					overPos += deltaDir;
					overlayLengthAcc += stepSize;
					if (overSam.a > 0.0) {
						maxA = max(maxA, overSam.a);
						overSample = overSample + overSam;
						isSurface += 1;
					}
				}
				if (isSurface > 0.0) {
					overSample = overSample/isSurface;
					overSample.a = maxA * customFragUniforms->overlayOpacity;
				}
				isSurface += 1.0;
			}
			colorSample.rgb = mix(colorSample.rgb, overSample.rgb, overSample.a);
			//end overlay()

			float3 a = colorSample.rgb * ambient;
			float s =  0;
			float3 d = float3(0.0, 0.0, 0.0);
			if (lengthAcc > stepSizeX2) {
				//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
				gradSample = (gradTexture.sample(textureSampler, samplePos)).rgba;
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				float lightNormDot = dot(gradSample.rgb, lightPosition);
				d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
				s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
			}
			colorSample.rgb = a + d + s;
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
		}
		samplePos += deltaDir;
		lengthAcc += stepSize;
		if ( lengthAcc >= len || colAcc.a > 0.95 )
			break;
	} //while lengthAcc < len
	colAcc.a = colAcc.a/0.95;
	colAcc.a *= fragUniforms->backAlpha;
    return float4(colAcc);
}
