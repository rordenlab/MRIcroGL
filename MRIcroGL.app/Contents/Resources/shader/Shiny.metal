//pref
//ambient|float|0.0|1.0|1
//diffuse|float|0.0|0.25|1
//specular|float|0.0|0.3|1
//shininess|float|0.01|10.0|30
//overlayFuzzy|float|0.01|0.5|1
//overlayDepth|float|0.0|0.3|0.8
//overlayClip|float|0|0|1

//vert
#include <metal_stdlib>
//xcrun -sdk macosx metal -c Default.metal -o Render.air

using namespace metal;

struct CustomFragUniforms {
float ambient;
float diffuse;
float specular;
float shininess;
float overlayFuzzy;
float overlayDepth;
float overlayClip;
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
	float clipThick;
	float backAlpha, pad1, pad2, pad3;
	float4 rayDir;
	float4 lightPos;
	float4 clipPlane;
	float4x4 normMatrix, modelViewProjectionMatrix;
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

#ifdef CUBIC
float4 texture(texture3d<float> vol, float3 coord) {
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear, address::clamp_to_zero);
	return (vol.sample(textureSampler, coord));			
}

/*
https://github.com/DannyRuijters/CubicInterpolationWebGL
License applicable to function texture3D():
Copyright (c) 2008-2013, Danny Ruijters. All rights reserved.
Ported to Apple Metal API 2020 by Chris Rorden

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
*  Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
*  Neither the name of the copyright holders nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/
float4 texture3Df(texture3d<float> vol, float3 coord) {
  // shift the coordinate from [0,1] to [-0.5, textureSz-0.5]
  float3 textureSz = float3(vol.get_width(), vol.get_height(), vol.get_depth());
  float3 coord_grid = coord * textureSz - 0.5;
  float3 index = floor(coord_grid);
  float3 fraction = coord_grid - index;
  float3 one_frac = 1.0 - fraction;
  float3 w0 = 1.0/6.0 * one_frac*one_frac*one_frac;
  float3 w1 = 2.0/3.0 - 0.5 * fraction*fraction*(2.0-fraction);
  float3 w2 = 2.0/3.0 - 0.5 * one_frac*one_frac*(2.0-one_frac);
  float3 w3 = 1.0/6.0 * fraction*fraction*fraction;
  float3 g0 = w0 + w1;
  float3 g1 = w2 + w3;
  float3 mult = 1.0 / textureSz;
  
  float3 h0 = mult * ((w1 / g0) - 0.5 + index);  //h0 = w1/g0 - 1, move from [-0.5, textureSz-0.5] to [0,1]
  float3 h1 = mult * ((w3 / g1) + 1.5 + index);  //h1 = w3/g1 + 1, move from [-0.5, textureSz-0.5] to [0,1]
  // fetch the eight linear interpolations
  // weighting and fetching is interleaved for performance and stability reasons
  float4 tex000 =  texture(vol,h0);
  float4 tex100 =  texture(vol,float3(h1.x, h0.y, h0.z));
  tex000 = mix(tex100, tex000, g0.x);  //weigh along the x-direction
  float4 tex010 =  texture(vol,float3(h0.x, h1.y, h0.z));
  float4 tex110 =  texture(vol,float3(h1.x, h1.y, h0.z));
  tex010 = mix(tex110, tex010, g0.x);  //weigh along the x-direction
  tex000 = mix(tex010, tex000, g0.y);  //weigh along the y-direction
  float4 tex001 =  texture(vol,float3(h0.x, h0.y, h1.z));
  float4 tex101 =  texture(vol,float3(h1.x, h0.y, h1.z));
  tex001 = mix(tex101, tex001, g0.x);  //weigh along the x-direction
  float4 tex011 =  texture(vol,float3(h0.x, h1.y, h1.z));
  float4 tex111 =  texture(vol,h1);
  tex011 = mix(tex111, tex011, g0.x);  //weigh along the x-direction
  tex001 = mix(tex011, tex001, g0.y);  //weigh along the y-direction
  return mix(tex001, tex000, g0.z);  //weigh along the z-direction
}
#endif


struct FragmentOut {
	float4 color [[color(0)]];
	float depth [[depth(any)]];
};

float setDepthBuffer(float3 pos, float4x4 mvp) {
	//return ((mvp * float4(pos, 1.0)).z + 1.0) * 0.5;
	return (mvp * float4(pos, 1)).z;
}

fragment FragmentOut fragmentShader(VertexOut  in [[stage_in]],
               texture3d<float> volTexture [[ texture(0) ]],
               texture3d<float> gradTexture [[ texture(1) ]],
               texture3d<float> overlayVolTexture [[ texture(2) ]],
               texture3d<float> overlayGradTexture [[ texture(3) ]],
			   const device FragUniforms* fragUniforms    	[[ buffer(1) ]],
               const device CustomFragUniforms* customFragUniforms    	[[ buffer(2) ]]
               ) {
	FragmentOut out;
	out.depth = 1000.0;
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear, address::clamp_to_zero);
	float2 gl_FragCoord = float2(in.position.xy); //random jitter to reduce wood grain
	float3 lightPosition = fragUniforms->lightPos.xyz;
	float clipThick = fragUniforms->clipThick;
	float backAlpha = fragUniforms->backAlpha;
	int overlays = round(fragUniforms->overlayNum);
	float ambient = customFragUniforms->ambient;
	float diffuse = customFragUniforms->diffuse;
	float specular = customFragUniforms->specular;
	float shininess = customFragUniforms->shininess;
	float overlayFuzzy = customFragUniforms->overlayFuzzy;
	float overlayDepth = customFragUniforms->overlayDepth;
	float overlayClip = customFragUniforms->overlayClip;	
	//float3x3 normalMatrix = float3x3(fragUniforms->normMatrix[0].xyz, fragUniforms->normMatrix[1].xyz, fragUniforms->normMatrix[2].xyz);
	float sliceSize = fragUniforms->sliceSiz;//for opacity correction
	float stepSize = fragUniforms->stepSiz;//sampling rate
	float4 clipPlane = fragUniforms->clipPlane;
	float3 start = in.color.rgb;
	float3 backPosition = GetBackPosition(start, fragUniforms->rayDir.xyz);
	float3 dir = backPosition - start;
	float len = length(dir);
	dir = normalize(dir);
	float4 deltaDir = float4(dir.xyz * stepSize, stepSize);
	float4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	float4 colAcc = float4(0.0,0.0,0.0,0.0);
	float4 prevGrad = float4(0.0,0.0,0.0,0.0);
	//background pass
	float noClipLen = len;
	float4 samplePos = float4(start.xyz, 0.0);
	//start applyClip(): Apple Metal does not support inout, so classic C
	float cdot = dot(dir,clipPlane.xyz);
	if  ((clipPlane.a > 1.0) || (cdot == 0.0)) {
		//return samplePos;'
	} else {
		bool frontface = (cdot > 0.0);
		float dis = (-clipPlane.a - dot(clipPlane.xyz, samplePos.xyz-0.5)) / cdot;
		float  disBackFace = (-(clipPlane.a-clipThick) - dot(clipPlane.xyz, samplePos.xyz-0.5)) / cdot;
		if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0)))
			samplePos.a = len + 1.0;
		else if (frontface) {
			dis = max(0.0, dis);
			samplePos = float4(samplePos.xyz+dir * dis, dis);
			len = min(disBackFace, len);
		} else {
			len = min(dis, len);
			disBackFace = max(0.0, disBackFace);
			samplePos = float4(samplePos.xyz+dir * disBackFace, disBackFace);
		}
	}
	float4 clipPos = samplePos;
	//end: applyClip()
	float opacityCorrection = stepSize/sliceSize;
	//fast pass - optional
	float4 deltaDirX2 = float4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));
	while  ( volTexture.sample(textureSampler, samplePos.xyz).a == 0.0) {
			samplePos += deltaDirX2;
			if (samplePos.a > len) break;
	}
	samplePos -= deltaDirX2;
	//end fast pass

	//if ((samplePos.a > len) && ( !hasOverlays )) { //no hit: quit here
	if ((samplePos.a > len) && ( overlays < 1 )) {
		out.color = colAcc;
		return out;	
	}	
	if (samplePos.a < clipPos.a) {
		samplePos = clipPos;
		bgNearest = clipPos.a;
		float stepSizeX2 = samplePos.a + (stepSize * 2.0);
		while (samplePos.a <= stepSizeX2) {
			colorSample = (volTexture.sample(textureSampler, samplePos.xyz));
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			samplePos += deltaDir;
		}
	
	}
	//end fastpass - optional
	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	samplePos += deltaDir * ran;
	//float3 defaultDiffuse = float3(0.5, 0.5, 0.5);
	int nHit = 0;
	while (samplePos.a <= len) {
		#ifdef CUBIC
		colorSample = texture3Df(volTexture, samplePos.xyz);
		#else
		colorSample = (volTexture.sample(textureSampler, samplePos.xyz));
		#endif
		if (colorSample.a > 0.0)  {
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			if (nHit == 0) {
				out.depth = setDepthBuffer(samplePos.xyz, fragUniforms->modelViewProjectionMatrix);
				nHit += 1;
				bgNearest = samplePos.a;
			}
			#ifdef CUBIC
			gradSample = texture3Df(gradTexture, samplePos.xyz);
			#else
			gradSample = (gradTexture.sample(textureSampler, samplePos.xyz)).rgba;
			#endif
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
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			if ( colAcc.a > 0.95 )
				break;

		}
		samplePos += deltaDir;
	}
	colAcc.a = colAcc.a/0.95;
	colAcc.a *= backAlpha;
	if ( overlays< 1 ) {
		out.color = colAcc;
		return out;
	}
	//overlay pass
	float overFarthest = len;
	float4 overAcc = float4(0.0,0.0,0.0,0.0);
	prevGrad = float4(0.0,0.0,0.0,0.0);
	if (overlayClip > 0)
		samplePos = clipPos;
	else {
		len = noClipLen;
		samplePos = float4(start.xyz +deltaDir.xyz* ran, 0.0);
	}
	//fast pass - optional
	clipPos = samplePos;
	deltaDirX2 = float4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));
	while  ( overlayVolTexture.sample(textureSampler, samplePos.xyz).a == 0.0) {
			samplePos += deltaDirX2;
			if (samplePos.a > len) break;
	}
	samplePos -= deltaDirX2;
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	//deltaDir = float4(dir.xyz * stepSize, stepSize);
	//end fastpass - optional
	while (samplePos.a <= len) {
		#ifdef CUBIC
		colorSample = texture3Df(overlayVolTexture,samplePos.xyz);
		#else
		colorSample = (overlayVolTexture.sample(textureSampler, samplePos.xyz));
		#endif
		if (colorSample.a > 0.00) {
			if (overAcc.a < 0.3)
				overFarthest = samplePos.a;
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			colorSample.a *=  overlayFuzzy;
			//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
			#ifdef CUBIC
			gradSample = texture3Df(overlayGradTexture,samplePos.xyz);
			#else
			gradSample = (overlayGradTexture.sample(textureSampler, samplePos.xyz));
			#endif
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
			colorSample.rgb *= colorSample.a;
			overAcc= (1.0 - overAcc.a) * colorSample + overAcc;
			if (overAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	overAcc.a = overAcc.a/0.95;
	float overMix = overAcc.a;
	if (((overFarthest) > bgNearest) && (colAcc.a > 0.0)) { //background (partially) occludes overlay
		float dx = (overFarthest - bgNearest)/1.73;
		dx = colAcc.a * pow(dx, overlayDepth);
		overMix *= 1.0 - dx;
	}
	colAcc.rgb = mix(colAcc.rgb, overAcc.rgb, overMix);
	colAcc.a = max(colAcc.a, overAcc.a);
	out.color = colAcc;
	return out;
}
