//pref
overlayClip|float|0|0|1|Does clipping also influence overlay layers?
//frag
//in theory, we could write to gl_FragDepth, but getting XYZ simpler than scalar depth
uniform float overlayClip = 0.0;

void main() {
    vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	vec3 dir = backPosition - start;
	float len = length(dir);
	dir = normalize(dir);
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
			colAcc = vec4(samplePos.xyz, 1.0);
			bgNearest = samplePos.a;
			break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
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
			overAcc = vec4(samplePos.xyz, 1.0);
			overFarthest = samplePos.a;
			break;
		}
		samplePos += deltaDir;
		if (samplePos.a > bgNearest) break;
	} //while samplePos.a < len
	if (overFarthest < bgNearest)
		colAcc = overAcc;
	#if ( __VERSION__ > 300 )
	FragColor = colAcc;
	#else
	gl_FragColor = colAcc;
	#endif
}