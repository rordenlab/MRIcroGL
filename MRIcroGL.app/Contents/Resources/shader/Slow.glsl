//pref
ambient|float|0.0|1.0|1
diffuse|float|0.0|0.25|1
specular|float|0.0|0.3|1
shininess|float|0.01|10.0|30
boundThresh|float|0.0|0.5|0.95
edgeBoundMix|float|0|0|1
overlayDistance|float|0.0|0.35|1
overlayShade|float|0.01|0.3|1.0
overlayClip|float|0|0|1
//frag
uniform float ambient = 1.0;
uniform float diffuse = 0.25;
uniform float specular = 0.3;
uniform float shininess = 10.0;
uniform float boundThresh = 0.5;
uniform float edgeBoundMix = 0.0;
uniform float overlayDistance = 0.0;
uniform float overlayShade = 0.0;
uniform float overlayClip = 0.0;

void main() {
	float edgeThresh = 0.01;
	float edgeExp = 0.5;
	float boundAcc = 0.0;
	float boundAcc2 = 0.0;
	float opacityCorrection = stepSize/sliceSize;
	vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	vec3 lightDirHeadOn = rayDir.xyz;
	vec3 dir = backPosition - start;
	float len = length(dir);
	dir = normalize(dir);
	gl_FragDepth = 1.0;
	int nHit = 0;	
	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);
	vec4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	float overNearest = len;
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	vec4 oprevGrad = vec4(0.0,0.0,0.0,0.0);
	
	vec4 samplePos;
	//background pass
	float noClipLen = len;
	samplePos = vec4(start.xyz, 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
	float stepSizeX2 = samplePos.a + (stepSize * 2.0);
	//fast pass - optional
	fastPass (len, dir, intensityVol, samplePos);
	#if ( __VERSION__ > 300 )
	if ((samplePos.a > len) && ( overlays < 1 )) { //no hit
		FragColor = colAcc;
		return;
	}
	#else
	if ((textureSz.x < 1) || ((samplePos.a > len) && ( overlays < 1 ))) { //no hit
		gl_FragColor = colAcc;
		return;		
	}	
	#endif
	if (samplePos.a < clipPos.a) {
		samplePos = clipPos;
		bgNearest = clipPos.a;
		float stepSizeX2 = samplePos.a + (stepSize * 2.0);
		while (samplePos.a <= stepSizeX2) {
			colorSample = texture3Df(intensityVol,samplePos.xyz);
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			samplePos += deltaDir;
		}
	
	}
	//if (samplePos.a < clipPos.a)
	//	samplePos = clipPos;
	deltaDir = vec4(dir.xyz * stepSize, stepSize);
	

	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	samplePos += deltaDir * ran;
		
	float clipLen = len;
	if (overlays > 0) {
		if (overlayClip > 0)
			samplePos = clipPos;
		else {
			len = noClipLen;
			samplePos = vec4(start.xyz +deltaDir.xyz* ran, 0.0);
		}
	}	
	//end fastpass - optional
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	float alphaTerminate = 0.95;
	if (overlays > 0)
		alphaTerminate = 2.0; //force exhaustive search
	colorSample = vec4(0.0,0.0,0.0,0.0);
	vec3 lightPositionN = normalize(lightPosition);
	while (samplePos.a <= len) {
		if ((samplePos.a > clipPos.a) && (samplePos.a <= clipLen)) {
			colorSample = texture3Df(intensityVol,samplePos.xyz);
			if (colorSample.a > 0.0) {
				colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
				if (nHit < 1) {
					nHit ++;
					bgNearest = samplePos.a;
					setDepthBuffer(samplePos.xyz);
				}
				gradSample= texture3Df(gradientVol,samplePos.xyz);
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				//Edge shading - darken edges parallel with viewing direction
				float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
				
				float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradSample.a,0.3);
				if (edgeVal >= edgeThresh)
					colorSample.rgb = mix(colorSample.rgb, vec3(0.0,0.0,0.0), pow((edgeVal-edgeThresh)/(1.0-edgeThresh),4.0));


				lightNormDot = dot(gradSample.rgb, lightPositionN);
				vec3 a = colorSample.rgb * ambient;
				vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
				float s =   specular * pow(max(dot(reflect(lightPositionN, gradSample.rgb), dir), 0.0), shininess);
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
		}
		if (overlays > 0) {
			vec4 ocolorSample = texture3Df(intensityOverlay,samplePos.xyz);
			ocolorSample.a = 1.0-pow((1.0 - ocolorSample.a), opacityCorrection);
			if (ocolorSample.a > 0.0) {
				if (nHit < 1) {
					nHit ++;
					setDepthBuffer(samplePos.xyz);
				}
				gradSample = texture3Df(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				if (gradSample.a < oprevGrad.a)
					gradSample.rgb = oprevGrad.rgb;
				oprevGrad = gradSample;
				overNearest= min(overNearest, samplePos.a);
				//Edge shading - darken edges parallel with viewing direction
				float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
				float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradSample.a,overlayShade);
				ocolorSample.a = pow(ocolorSample.a, 1.0 -edgeVal);
				ocolorSample.rgb = mix(ocolorSample.rgb, vec3(0.0,0.0,0.0), edgeVal);

				lightNormDot = dot(gradSample.rgb, lightPositionN);

				vec3 a = ocolorSample.rgb * ambient;
				vec3 d = max(lightNormDot, 0.0) * ocolorSample.rgb * diffuse;
				float s =   specular * pow(max(dot(reflect(lightPositionN, gradSample.rgb), dir), 0.0), shininess);
				ocolorSample.rgb = a + d + s;
				
				if ( ocolorSample.a > 0.2) {
					float overRatio = colorSample.a/(ocolorSample.a);
					if (colorSample.a > 0.02)
						colorSample.rgb = mix( colorSample.rgb, ocolorSample.rgb, overRatio);
					else
						colorSample.rgb = ocolorSample.rgb;
					colorSample.a = max(ocolorSample.a, colorSample.a);
				}


				ocolorSample.a = 1.0-pow((1.0 - ocolorSample.a), opacityCorrection);
				overAcc= (1.0 - overAcc.a) * ocolorSample + overAcc;
				boundAcc2 += (1.0 - boundAcc2) * ocolorSample.a;
			}
		} //if hasOverlays
		
		
		colorSample.rgb *= colorSample.a;
		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
		if ( colAcc.a > alphaTerminate )
			break;
		samplePos += deltaDir;
	} //while samplePos.a < len
	colAcc.a *= backAlpha;
	if ((edgeBoundMix > 0.0) && ((colAcc.a + boundAcc) > 0.0)) {
		colAcc.rgb = mix(colAcc.rgb, vec3(0.0,0.0,0.0), (edgeBoundMix * boundAcc)/(colAcc.a+(edgeBoundMix * boundAcc)) );
		colAcc.a = max(colAcc.a, boundAcc);
	}
	if ((overlays < 1) || (overAcc.a == 0.0))  {
		#if ( __VERSION__ > 300 )
		FragColor = colAcc;
		#else
		gl_FragColor = colAcc;
		#endif
		return;
	}
	if (overNearest <= bgNearest) { //if overlay closer than background
		colAcc.rgb=mix(colAcc.rgb, overAcc.rgb,  overAcc.a);
		#if ( __VERSION__ > 300 )
		FragColor = colAcc;
		#else
		gl_FragColor = colAcc;
		#endif
		return;
	}
	//overlay behind surface
	float depth = (overNearest - bgNearest) / 1.732; //opposite corners of cube are 1.732 from each other
	depth = depth + overlayDistance;
	depth = min(depth, 1.0);
	depth = sqrt(depth);
	colAcc.rgb = mix(overAcc.rgb, colAcc.rgb,  depth);
	#if ( __VERSION__ > 300 )
	FragColor = colAcc;
	#else
	gl_FragColor = colAcc;
	#endif
}