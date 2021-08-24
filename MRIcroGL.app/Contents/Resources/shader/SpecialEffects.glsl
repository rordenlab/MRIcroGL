//pref
showGradient|float|0|0|1|Display surface angle
doPoor|float|0|1|1|Poor quality reveals rendering strategy
doJitter|float|0|0|1|Jitter hides wood-grain artifacts
showStartEnd|float|0|0.35|1|Show background box

//frag
uniform float showGradient = 0.0;
uniform float doPoor = 0.0;
uniform float doJitter = 0.0;
uniform float showStartEnd = 0.2;

void main() {
	float ambient = 1.0;
	float diffuse = 0.3;
	float specular = 0.25;
	float shininess = 10.0;
    vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	vec3 dir = backPosition - start;
	float ran = 0.0;
	if (doJitter > 0.5)
		ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	float len = length(dir);
	dir = normalize(dir);
	gl_FragDepth = 1.0;
	int nHit = 0;	
	float stepSizeX = stepSize;
	if (doPoor > 0.5) stepSizeX *= 10.0;
	vec4 deltaDir = vec4(dir.xyz * stepSizeX, stepSizeX);
	vec4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	//background pass
	vec4 samplePos = vec4(start.xyz, 0.0);
	samplePos += deltaDir * ran;
	vec4 clipPos = applyClip(dir, samplePos, len);
	#if ( __VERSION__ > 300 )
	if ( clipPos.a > len ) {
		FragColor = colAcc;
		return;
	}
	#else
	if ((textureSz.x < 1) || ( clipPos.a > len )) {
		gl_FragColor = colAcc;
		return;
	}
	#endif
	//	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	//
	if (doPoor <= 0.5)
		fastPass (len, dir, intensityVol, samplePos);
	//if (samplePos.a < clipPos.a)
	//	samplePos = clipPos;
		
	while (samplePos.a <= len) {
		colorSample = texture3Df(intensityVol,samplePos.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSizeX/sliceSize);
		if (colorSample.a > 0.0) {
			if (showGradient > 0.5)
				colorSample.rgb = abs(texture3Df(gradientVol,samplePos.xyz).rgb *2.0 - 1.0);
			if (nHit < 1) {
				nHit ++;
				bgNearest = samplePos.a;
				setDepthBuffer(samplePos.xyz);
			}
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			if ( colAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	colAcc.a = colAcc.a/0.95;
	colAcc.a *= backAlpha;
	if (samplePos.a > (len +0.5)) {
		//	
	} else if (showStartEnd < 0.2) {
		colAcc.rgb = mix(clipPos.xyz, colAcc.rgb, colAcc.a);
		colAcc.a = 1.0;
	} else if (showStartEnd < 0.4) {
		colAcc.rgb = mix(clipPos.xyz + (dir * (len - clipPos.a)), colAcc.rgb, colAcc.a);
		colAcc.a = 1.0;
	} else if (showStartEnd < 0.6) {
		colAcc = vec4(clipPos.xyz, 1.0);
	} else if (showStartEnd < 0.8) {
		colAcc = vec4(clipPos.xyz + (dir * (len - clipPos.a)), 1.0);
	}			
	if ( overlays < 1 ) {
		#if ( __VERSION__ > 300 )
		FragColor = colAcc;
		#else
		gl_FragColor = colAcc;
		#endif
		return;
	}
	//overlay pass
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	prevGrad = vec4(0.0,0.0,0.0,0.0);
	samplePos = clipPos;
	//fast pass - optional
	clipPos = samplePos;
	if (doPoor <= 0.5)
		fastPass (len, dir, intensityOverlay, samplePos);
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	//end fastpass - optional
	float overFarthest = len;
	while (samplePos.a <= len) {
		colorSample = texture3Df(intensityOverlay,samplePos.xyz);
		if (colorSample.a > 0.00) {
			if (overAcc.a < 0.3)
				overFarthest = samplePos.a;
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSizeX/sliceSize);
			vec3 a = colorSample.rgb * ambient;
			float s =  0;
			vec3 d = vec3(0.0, 0.0, 0.0);
			//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
			gradSample = texture3Df(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude
			gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
			//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
			if (gradSample.a < prevGrad.a)
				gradSample.rgb = prevGrad.rgb;
			prevGrad = gradSample;
			float lightNormDot = dot(gradSample.rgb, lightPosition);
			d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
			s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);

			colorSample.rgb = a + d + s;
			colorSample.rgb *= colorSample.a;
			overAcc= (1.0 - overAcc.a) * colorSample + overAcc;
			if (overAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	overAcc.a = overAcc.a/0.95;
	//end ovelay pass clip plane applied to background ONLY...
	//if (overAcc.a > 0.0) { //<- conditional not required: overMix always 0 for overAcc.a = 0.0
		float overMix = overAcc.a;
		if (((overFarthest) > bgNearest) && (colAcc.a > 0.0)) { //background (partially) occludes overlay
			float dx = (overFarthest - bgNearest)/1.73;
			float overlayDepth = 0.3;
			dx = colAcc.a * pow(dx, overlayDepth);
			overMix *= 1.0 - dx;
		}
		colAcc.rgb = mix(colAcc.rgb, overAcc.rgb, overMix);
		colAcc.a = max(colAcc.a, overAcc.a);
	//}
    #if ( __VERSION__ > 300 )
	FragColor = colAcc;
	#else
	gl_FragColor = colAcc;
	#endif
}