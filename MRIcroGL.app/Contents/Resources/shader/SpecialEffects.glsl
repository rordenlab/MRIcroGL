//pref
showGradient|float|0|0|1|Display surface angle
doPoor|float|0|1|1|Poor quality reveals rendering strategy
doJitter|float|0|0|1|Jitter hides wood-grain artifacts
showStartEnd|float|0|0.5|1|Show background box

//frag
uniform float showGradient = 0.0;
uniform float doPoor = 0.0;
uniform float doJitter = 0.0;
uniform float showStartEnd = 0.2;

void main() {
	#ifdef BETTER_BUT_SLOWER
	textureSz = textureSize(intensityVol, 0);
	#endif
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
	float stepSizeX = stepSize;
	if (doPoor > 0.5) stepSizeX *= 10.0;
	vec4 deltaDir = vec4(dir.xyz * stepSizeX, stepSizeX);
	vec4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	float overFarthest = len;
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	//background pass
	vec4 samplePos = vec4(start.xyz, 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
	if ( clipPos.a > len ) {
		FragColor = colAcc;
		return;
	}
	//	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	samplePos += deltaDir * ran;
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	//
	if (doPoor <= 0.5)
		fastPass (len, dir, intensityVol, samplePos);
	//if (samplePos.a < clipPos.a)
	//	samplePos = clipPos;
		
	while (samplePos.a <= len) {
		colorSample = texture3D(intensityVol,samplePos.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSizeX/sliceSize);
		if (colorSample.a > 0.0) {
			if (showGradient > 0.5) {
				//colorSample.rgb = abs(texture3D(gradientVol,samplePos.xyz).rgb);
				//colorSample.rgb = colorSample.rgb*2.0 - 1.0;
				//colorSample.rgb = abs(colorSample.rgb);
				
				colorSample.rgb = abs(texture3D(gradientVol,samplePos.xyz).rgb *2.0 - 1.0);
				
			}
			bgNearest = min(samplePos.a,bgNearest);
				//gradSample= texture3D(gradientVol,samplePos.xyz);
				//gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				//if (gradSample.a < prevGrad.a)
				//	gradSample.rgb = prevGrad.rgb;
				//prevGrad = gradSample;
				//vec3 a = colorSample.rgb * ambient;
				//float lightNormDot = dot(gradSample.rgb, lightPosition);
				//vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
				//float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
				//colorSample.rgb = a + d + s;

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
	} else if (showStartEnd < 0.33) {
		colAcc.rgb = mix(clipPos.xyz, colAcc.rgb, colAcc.a);
		colAcc.a = 1.0;
	} else if (showStartEnd < 0.66) {
		colAcc.rgb = mix(clipPos.xyz + (dir * (len - clipPos.a)), colAcc.rgb, colAcc.a);
		//colAcc.rgb = mix(clipPos.xyz + (dir * len), colAcc.rgb, colAcc.a);
		colAcc.a = 1.0;
	}			
	if ( overlays < 1 ) {
		FragColor = colAcc;
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
	while (samplePos.a <= len) {
		colorSample = texture3D(intensityOverlay,samplePos.xyz);
		if (colorSample.a > 0.00) {
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSizeX/sliceSize);
			vec3 a = colorSample.rgb * ambient;
			float s =  0;
			vec3 d = vec3(0.0, 0.0, 0.0);
			overFarthest = samplePos.a;
			//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
			gradSample = texture3D(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude
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
    FragColor = colAcc;
}