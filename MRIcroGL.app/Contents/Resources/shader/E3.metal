//pref
//ambient|float|0.0|1.0|1
//diffuse|float|0.0|0.3|1
//specular|float|0.0|0.25|1
//shininess|float|0.01|10.0|30
//boundThresh|float|0.0|0.5|0.95
//edgeBoundMix|float|0|0|1
//backAlpha|float|0.0|0.95|1
//overDistance|float|0.0|0.3|1
//overAlpha|float|0.0|1.0|2.0
//overShade|float|0.0|0.3|1.0
//vert
#include <metal_stdlib>
//xcrun -sdk macosx metal -c Default.metal -o Render.air

using namespace metal;

struct CustomFragUniforms {
	float ambient, diffuse, specular, shininess, boundThresh,edgeBoundMix,
	backAlpha,overDistance,overAlpha,overShade;

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
    float3 backPosition = GetBackPosition(start, fragUniforms->rayDir.xyz);
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
	bool hasOverlays = (fragUniforms->overlayNum > 0);
	float3 lightPosition = fragUniforms->lightPos.xyz;
	float ambient = customFragUniforms->ambient;
	float overDistance = customFragUniforms->overDistance;
	float diffuse = customFragUniforms->diffuse;
	float specular = customFragUniforms->specular;
	float shininess = customFragUniforms->shininess;
	float edgeBoundMix = customFragUniforms->edgeBoundMix;
	float3 lightDirHeadOn = fragUniforms->rayDir.xyz;
	float overShade = customFragUniforms->overShade;
	float boundThresh = customFragUniforms->boundThresh;
	float overAlpha = customFragUniforms->overAlpha;
	float overAlphaFrac = overAlpha;
	if (overAlphaFrac > 1.0) overAlphaFrac = 1.0;
	float3 deltaDir = dir * stepSize;
	float4 gradSample, colorSample,colAcc = float4(0.0,0.0,0.0,0.0);
	float4 overAcc = float4(0.0,0.0,0.0,0.0);
	float4 prevGrad = float4(0.0,0.0,0.0,0.0);
	float4 oprevGrad = float4(0.0,0.0,0.0,0.0);
	float lengthAcc = 0.0;
	float edgeThresh = 0.01;
	float edgeExp = 0.5;
	float3 samplePos = start.xyz;
	float overDepth = -1;
	float backDepthEnd, backDepthStart = -1;
	float2 gl_FragCoord = float2(in.position.xy); //random jitter to reduce wood grain
	//stochastic jitter http://www.mccauslandcenter.sc.edu/mricrogl/shaders
	samplePos += deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));
	float alphaTerminate = 0.95;
	float boundAcc = 0.0;
	float boundAcc2 = 0.0;
	float opacityCorrection = stepSize/sliceSize;
	if (hasOverlays)
		alphaTerminate = 2.0; //force exhaustive search
	while (lengthAcc <= len) {
		if ((lengthAcc <= clipStart) || (lengthAcc > clipEnd))
			colorSample = float4(0.0,0.0,0.0,0.0);
		else {
			colorSample = (volTexture.sample(textureSampler, samplePos));
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			if ((colorSample.a > 0.01) && (lengthAcc > stepSizeX2)) {
				if (backDepthStart < 0) backDepthStart = lengthAcc;
				backDepthEnd = lengthAcc;
				//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
				gradSample = (gradTexture.sample(textureSampler, samplePos)).rgba;
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				//Edge shading - darken edges parallel with viewing direction
				float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
				float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradSample.a,0.3);
				if (edgeVal >= edgeThresh)
					colorSample.rgb = mix(colorSample.rgb, float3(0.0,0.0,0.0), pow((edgeVal-edgeThresh)/(1.0-edgeThresh),4.0));


				lightNormDot = dot(gradSample.rgb, lightPosition);
				float3 a = colorSample.rgb * ambient;
				float3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
				float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
				//
				if (gradSample.a > boundThresh) {
					float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
					float boundAlpha = pow(1.0-abs(lightNormDot),6.0);
					boundAlpha = 1.0-pow((1.0 - boundAlpha), opacityCorrection);
					boundAcc += (1.0 - boundAcc2) * boundAlpha;
					boundAcc2 += (1.0 - boundAcc2) * boundAlpha;
				}

				colorSample.rgb = a + d + s;
			}
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
		}
		if (hasOverlays) {
			colorSample.rgba = (overlayVolTexture.sample(textureSampler, samplePos));
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			if (colorSample.a > 0.01) {
				gradSample = (overlayGradTexture.sample(textureSampler, samplePos)).rgba;
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				if (gradSample.a < oprevGrad.a)
					gradSample.rgb = oprevGrad.rgb;
				oprevGrad = gradSample;
				if (overDepth < 0) overDepth = lengthAcc;
				//Edge shading - darken edges parallel with viewing direction
				float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
				float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradSample.a,overShade);
				colorSample.a = pow(colorSample.a, 1.0 -edgeVal);
				colorSample.rgb = mix(colorSample.rgb, float3(0.0,0.0,0.0), edgeVal);

				lightNormDot = dot(gradSample.rgb, lightPosition);

				float3 a = colorSample.rgb * ambient;
				float3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
				float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
				colorSample.rgb = a + d + s;
				colorSample.a *= overAlphaFrac;

				colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
				overAcc= (1.0 - overAcc.a) * colorSample + overAcc;
				boundAcc2 += (1.0 - boundAcc2) * colorSample.a;

			}
		}
		samplePos += deltaDir;
		lengthAcc += stepSize;
		if ( lengthAcc >= len || colAcc.a > alphaTerminate )
			break;
	}

	if ((edgeBoundMix > 0.0) && ((colAcc.a + boundAcc) > 0.0)) {
		colAcc.rgb = mix(colAcc.rgb, float3(0.0,0.0,0.0), (edgeBoundMix * boundAcc)/(colAcc.a+(edgeBoundMix * boundAcc)) );
		colAcc.a = max(colAcc.a, boundAcc);
	}
	if ((overAcc.a > 0.01) && (overAlpha > 1.0))  {
		colAcc.a=max(colAcc.a,overAcc.a);
		if ( (overDistance > 0.0) && (overDepth > backDepthStart) && (backDepthEnd > backDepthStart)) {
			if (overDepth > backDepthEnd) overDepth = backDepthStart; // backDepthEnd
			float dx = float(overDepth-backDepthStart)/ float(backDepthEnd - backDepthStart);
			dx = pow(1.0-dx, overDistance);
			dx = pow(dx, 2.0);
			overAcc *= dx;
		}
		//overAlphaFrac = (overAlpha - 1.0);
		overAlphaFrac = overAcc.a * (overAlpha - 1.0);
		if (overAcc.a > 0.0)
			colAcc.rgb=mix(colAcc.rgb, overAcc.rgb,  overAlphaFrac);
	}


	//if ( colAcc.a < 1.0 )
	//	colAcc.rgb = mix(clearColor,colAcc.rgb,colAcc.a);

	//colAcc = ocolAcc;
	//colAcc.a = colAcc.a/0.95;
    return float4(colAcc);
}
