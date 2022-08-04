//pref
matCapThreshold|float|0.0|0.4|2.0
overlayFuzzy|float|0.01|0.5|1
overlayDepth|float|0.0|0.15|0.99
overlayClip|float|0|0|1|Does clipping also influence overlay layers?
//frag
uniform float matCapThreshold = 0.5;
uniform float overlayFuzzy = 0.5;
uniform float overlayDepth = 0.3;
uniform float overlayClip = 0.0;

uniform mat3 NormalMatrix;
uniform sampler2D matcap2D;

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
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	//background pass
	float noClipLen = len;
	vec4 samplePos = vec4(start.xyz, 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
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
	vec4 colorMax = vec4(0.0, 0.0, 0.0, 0.0);
	if (samplePos.a < clipPos.a) {
		samplePos = clipPos;
		if (nHit < 1) {
			nHit ++;
			bgNearest = clipPos.a;
			setDepthBuffer(samplePos.xyz);
		}
		
		float stepSizeX2 = samplePos.a + (stepSize * 2.0);
		while (samplePos.a <= stepSizeX2) {
			colorSample = texture3Df(intensityVol,samplePos.xyz);
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
			if (colorSample.a > colorMax.a) colorMax = colorSample;
			colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			samplePos += deltaDir;
		}
		if ( colAcc.a > 0.95 ) samplePos.a = len + deltaDir.a;
	}
	//end fastpass - optional
	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	samplePos += deltaDir * ran;
	//start tomography
	vec4 gradAcc = vec4(0.0, 0.0, 0.0, 0.0);
	vec4 gradMax = gradAcc;
	//end tomography
	vec3 defaultDiffuse = vec3(0.75, 0.75, 0.75);
	while (samplePos.a <= len) {
		colorSample = texture3Df(intensityVol,samplePos.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
		if (colorSample.a > 0.01) {
			if (colorSample.a > colorMax.a) colorMax = colorSample;
				if (nHit < 1) {
					nHit ++;
					bgNearest = samplePos.a;
					setDepthBuffer(samplePos.xyz);
				}
				vec3 a = colorSample.rgb;
				gradSample= texture3Df(gradientVol,samplePos.xyz);
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				if (gradSample.a > gradMax.a)
					gradMax = gradSample;
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				gradAcc = (1.0 - gradAcc.a) * prevGrad + gradAcc;
				//vec3 n = normalize(NormalMatrix * gradSample.rgb);
				//vec2 uv = n.xy * 0.5 + 0.5;
				//vec3 d = texture2D(matcap2D,uv.xy).rgb;
				//vec3 surf = mix(defaultDiffuse, a, surfaceColor); //0.67 as default Brighten is 1.5
				//colorSample.rgb = d * surf * brighten;
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			if ( colAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	//start tomography
	float alphaThresh = 0.95;
	gradSample = gradMax;//mix(gradAcc, gradMax, gradientMix);
	float matCapFrac = smoothstep( matCapThreshold - 0.2, matCapThreshold+0.2, gradSample.a);
	if (matCapThreshold > 1.0)
		matCapFrac = smoothstep( (2.0 - matCapThreshold) - 0.2, (2.0 - matCapThreshold)+0.2, 1.0 - gradSample.a);
	if (matCapFrac > 0.0) {
		colorSample = texture3Df(intensityVol,samplePos.xyz);
		vec3 n = normalize(NormalMatrix * gradSample.rgb);
		vec2 uv = n.xy * 0.5 + 0.5;
		vec3 d = texture2D(matcap2D,uv.xy).rgb;
		//vec3 surf = mix(defaultDiffuse, colorSample.rgb, surfaceColor); //0.67 as default Brighten is 1.5
		//colorSample.rgb = d * surf * (brighten / 2.0);
		colorSample.rgb = d;// *  (brighten / 2.0);
		colAcc.rgb = mix(colAcc.rgb, colorSample.rgb,  matCapFrac);
	}
	//end tomography
	colAcc.a = colAcc.a/0.95;
	colAcc.a *= backAlpha;
	if ( overlays< 1 ) {
		#if ( __VERSION__ > 300 )
		FragColor = colAcc;
		#else
		gl_FragColor = colAcc;
		#endif
		return;
	}
	//overlay pass
	float ambient = 1.0;
	float diffuse = 0.3;
	float specular = 0.25;
	float shininess = 10.0;

	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	prevGrad = vec4(0.0,0.0,0.0,0.0);
	if (overlayClip > 0)
		samplePos = clipPos;
	else {
		len = noClipLen;
		samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);
	}
	//fast pass - optional
	clipPos = samplePos;
	fastPass (len, dir, intensityOverlay, samplePos);
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	//end fastpass - optional
	float overFarthest = len;
	vec3 lightPositionN = normalize(lightPosition);
	while (samplePos.a <= len) {
		colorSample = texture3Df(intensityOverlay,samplePos.xyz);
		if (colorSample.a > 0.00) {
			if (nHit < 1) {
				nHit ++;
				setDepthBuffer(samplePos.xyz);
			}
			if (overAcc.a < 0.3)
				overFarthest = samplePos.a;
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
			colorSample.a *=  overlayFuzzy;
			//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
			gradSample = texture3Df(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude
			gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
			//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
			if (gradSample.a < prevGrad.a)
				gradSample.rgb = prevGrad.rgb;
			prevGrad = gradSample;
			float lightNormDot = dot(gradSample.rgb, lightPositionN);
			vec3 a = colorSample.rgb * ambient;
			vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
			float s =   specular * pow(max(dot(reflect(lightPositionN, gradSample.rgb), dir), 0.0), shininess);
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