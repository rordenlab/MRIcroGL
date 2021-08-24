//pref
overlayFuzzy|float|0.01|0.5|1|Do overlay layers have blurry surfaces?
overlayDepth|float|0.0|0.3|0.8|Can we see overlay layers deep beneath the background image?
overlayClip|float|0|0|1|Does clipping also influence overlay layers?
//frag
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
	vec4 colorSample;
	float bgNearest = len; //assume no hit
	float overFarthest = len;
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 samplePos;
	//background pass
	float noClipLen = len;
	samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
	float stepSizeX2 = samplePos.a + (stepSize * 2.0);
	while (samplePos.a <= len) {
		colorSample = texture3Df(intensityVol,samplePos.xyz);
		if (colorSample.a > 0.0) {
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
			if (nHit < 1) {
				nHit ++;
				bgNearest = samplePos.a;
				setDepthBuffer(samplePos.xyz);
			}
			if (samplePos.a < stepSizeX2)
				colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			if ( colAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	colAcc.a = colAcc.a/0.95;
	colAcc.a *= backAlpha;
	#if ( __VERSION__ > 300 )
	if ( overlays < 1 ) {
		FragColor = colAcc;
		return;
	}
	#else
	if ((textureSz.x < 1) || ( overlays < 1 )) {
		gl_FragColor = colAcc;
		return;
	}
	#endif
	
	//overlay pass
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	if (overlayClip > 0)
		samplePos = clipPos;
	else {
		len = noClipLen;
		samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);
	}
	while (samplePos.a <= len) {
		colorSample = texture3Df(intensityOverlay,samplePos.xyz);
		if (colorSample.a > 0.00) {
			if (nHit < 1) {
				nHit ++;
				setDepthBuffer(samplePos.xyz);
			}
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
			colorSample.a *=  overlayFuzzy;
			overFarthest = samplePos.a;
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