//pref
boundThresh|float|0.0|0.4|0.95|Boundary threshold (requires high edgeBoundMix)
edgeThresh|float|0|0.1|1
edgeBoundMix|float|0|0|1
colorTemp|float|0|0.5|1
specular|float|0|0.5|1
overlayFuzzy|float|0.01|0.5|1
overlayDepth|float|0.0|0.15|0.99
overlayClip|float|0|0|1|Does clipping also influence overlay layers?
//frag
uniform float boundThresh = 0.95;
uniform float edgeThresh = 0.4;
uniform float edgeBoundMix = 0.9;
uniform float colorTemp = 0.9;
uniform float specular = 0.5;
uniform float overlayFuzzy = 0.5;
uniform float overlayDepth = 0.3;
uniform float overlayClip = 0.0;

void main() {
	vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	vec3 dir = backPosition - start;
	float len = length(dir);
	dir = normalize(dir);
	gl_FragDepth = 1.0;
	int nHit = 0;	
	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);
	vec4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	float ambient = 1.0;
	float diffuse = 0.3;
	float shininess = 7.0;
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	//background pass
	float noClipLen = len;
	vec4 samplePos = vec4(start.xyz, 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
	float opacityCorrection = stepSize/sliceSize;
	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	//fast pass - optional
	fastPass (len, dir, gradientVol, samplePos);
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
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	//end fastpass - optional
	gradSample = texture3Df(intensityVol,samplePos.xyz); //only to ensure this is used
	samplePos += deltaDir * ran;
	vec3 lightDirHeadOn = dir;
	float boundAcc = 0.0;
	vec4 edgeColor = vec4(1.0,1.0,1.0, 0.0);
	if (colorTemp < 0.5) {
		edgeColor.b = 1.0-((0.5-colorTemp)*-0.5);
		edgeColor.g = 1.0-((0.5-colorTemp)*-0.1);
	}
	if (colorTemp > 0.5) {
		edgeColor.g = 1.0-((colorTemp-0.5)*-0.1);
		edgeColor.r = 1.0-((colorTemp-0.5)*-0.5);
	}
	float edgeExp = 0.2;
		
	while (samplePos.a <= len) {
			gradSample = texture3Df(gradientVol,samplePos.xyz);
			samplePos += deltaDir;
			if (gradSample.a < 0.01)
				continue;
			if (nHit < 1) {
				nHit ++;
				bgNearest = samplePos.a;
				setDepthBuffer(samplePos.xyz);
			}
			if (gradSample.a < prevGrad.a)
				gradSample.rgb = prevGrad.rgb;
			prevGrad = gradSample;
			colorSample = gradSample;
			gradSample.rgb = gradSample.rgb*2.0 - 1.0; //do not normalize: vec3(0,0,0)!!
			if (gradSample.a > boundThresh) {
					float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
					float boundAlpha = pow(1.0-abs(lightNormDot),6.0);
					boundAlpha = 1.0-pow((1.0 - boundAlpha), opacityCorrection);
					boundAcc += (1.0 - boundAcc) * boundAlpha;
			}
			colorSample = edgeColor; //color with alpha = 0.0
			if  (gradSample.a > edgeThresh) {
				float edge = smoothstep(edgeThresh, 1.0, gradSample.a);
				edge = pow(edge,edgeExp);
				colorSample.a = edge;
				//vec3 n = normalize(normalize(NormalMatrix * gradSample.rgb));
				//vec2 uv = n.xy * 0.5 + 0.5;
				//colorSample.rgb = texture2D(matcap2D,uv.xy).rgb * brighten;
				float lightNormDot = dot(gradSample.rgb, lightPosition); //with respect to light location
				if (lightNormDot > 0.0) edge +=   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
				colorSample.rgb = edgeColor.rgb * edge;
			}
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			colorSample.rgb *= colorSample.a;
			//accumulate color
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
	} //while samplePos.a < len
	if ((edgeBoundMix > 0.0) && ((colAcc.a + boundAcc) > 0.0)) {
		colAcc.rgb = mix(colAcc.rgb, vec3(0.0,0.0,0.0), (edgeBoundMix * boundAcc)/(colAcc.a+(edgeBoundMix * boundAcc)) );
		colAcc.a = max(colAcc.a, boundAcc);
	}
	colAcc.a *= backAlpha;
	#if ( __VERSION__ > 300 )
	FragColor = colAcc;
	#else
	gl_FragColor = colAcc;
	#endif
	if ( overlays< 1 )	
		return;
	
	//overlay pass
		if (overlayClip > 0)
		samplePos = clipPos;
	else {
		len = noClipLen;
		samplePos = vec4(start.xyz +deltaDir.xyz* ran, 0.0);
	}
	//fast pass - optional
	clipPos = samplePos;
	fastPass (len, dir, intensityOverlay, samplePos);
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	//end fastpass - optional
		vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
		vec4 oprevGrad = vec4(0.0,0.0,0.0,0.0);
		float overFarthest = len;
		while (samplePos.a <= len) {
			colorSample = texture3Df(intensityOverlay,samplePos.xyz);//2020TODO -HQ?
			samplePos += deltaDir;
			if (colorSample.a < 0.01)
				continue;
			if (nHit < 1) {
				nHit ++;
				setDepthBuffer(samplePos.xyz);
			}
			if (overAcc.a < 0.3)
				overFarthest = samplePos.a;
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			colorSample.a *=  overlayFuzzy;
			vec3 a = colorSample.rgb * ambient;
			float s =  0;
			vec3 d = vec3(0.0, 0.0, 0.0);
			//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
			gradSample = texture3Df(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude
			gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
			//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
			if (gradSample.a < oprevGrad.a)
				gradSample.rgb = oprevGrad.rgb;
			oprevGrad = gradSample;
			float lightNormDot = dot(gradSample.rgb, lightPosition);
			d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
			s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
			colorSample.rgb = a + d + s;
			colorSample.rgb *= colorSample.a;
			overAcc= (1.0 - overAcc.a) * colorSample + overAcc;
			if (overAcc.a > 0.95 )
				break;
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
		#if ( __VERSION__ > 300 )
		FragColor = colAcc;
		#else
		gl_FragColor = colAcc;
		#endif
}