//pref
brighten|float|0.5|2|3.5
surfaceColor|float|0.0|1.0|1.0
boundThresh|float|0.0|0.5|0.95
edgeBoundMix|float|0|0.9|1
overlayFuzzy|float|0.01|0.5|1
overlayDepth|float|0.0|0.15|0.99
overlayClip|float|0|0|1|Does clipping also influence overlay layers?
//frag
uniform float brighten = 1.5;
uniform float surfaceColor = 1.0;
uniform float boundThresh = 0.95;
uniform float edgeBoundMix = 0.9;
uniform float overlayDepth = 0.3;
uniform float overlayFuzzy = 0.5;
uniform float overlayClip = 0.0;

uniform mat3 NormalMatrix;
uniform sampler2D matcap2D;

void main() {
	#ifdef BETTER_BUT_SLOWER
	textureSz = textureSize(intensityVol, 0);
	#endif
    vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	vec3 dir = backPosition - start;
	float len = length(dir);
	dir = normalize(dir);
	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);
	vec4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	float overFarthest = len;
	float ambient = 1.0;
	float diffuse = 0.3;
	float specular = 0.25;
	float shininess = 10.0;
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	//background pass
	float noClipLen = len;
	vec4 samplePos = vec4(start.xyz, 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
	float opacityCorrection = stepSize/sliceSize;
	//fast pass - optional
	fastPass (len, dir, intensityVol, samplePos);
	if ((samplePos.a > len) && ( overlays < 1 )) { //no hit: quit here
		FragColor = colAcc;
		return;		
	}
	if (samplePos.a < clipPos.a) {
		samplePos = clipPos;
		bgNearest = clipPos.a;
		float stepSizeX2 = samplePos.a + (stepSize * 2.0);
		while (samplePos.a <= stepSizeX2) {
			colorSample = texture3D(intensityVol,samplePos.xyz);
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
	float boundAcc = 0.0;
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	while (samplePos.a <= len) {
		gradSample = texture3D(gradientVol,samplePos.xyz);
		samplePos += deltaDir;
		//next: a hack. e.g. T1 deep white matter might be saturated, bright but no gradient
		//  the stepSizeX2 previously attempts to make clip planes opque to hide this
		if (gradSample.a == 0.0) continue;
		gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
		//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
		//if (gradSample.a < prevGrad.a)
		//	gradSample.rgb = prevGrad.rgb;
		//prevGrad = gradSample;
		if (gradSample.a > boundThresh) {
			float lightNormDot = dot(gradSample.rgb, dir); //with respect to viewer
			float boundAlpha = pow(1.0-abs(lightNormDot),6.0);
			boundAlpha = 1.0-pow((1.0 - boundAlpha), opacityCorrection);
			boundAcc += (1.0 - boundAcc) * boundAlpha;
		}
		if (colAcc.a > 0.95) continue;
		colorSample = texture3D(intensityVol,samplePos.xyz-deltaDir.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
		bgNearest = min(samplePos.a,bgNearest);
		vec3 n = normalize(normalize(NormalMatrix * gradSample.rgb));
		vec3 d = texture(matcap2D, n.xy * 0.5 + 0.5).rgb;
		colorSample.rgb = mix(defaultDiffuse, colorSample.rgb, surfaceColor); //0.67 as default Brighten is 1.5
			
		colorSample.rgb *= d * brighten * colorSample.a;
		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
	} //while samplePos.a < len
	colAcc.a = colAcc.a/0.95;
	
	if ((edgeBoundMix > 0.0) && ((colAcc.a + boundAcc) > 0.0)) {
		colAcc.rgb = mix(colAcc.rgb, vec3(0.0,0.0,0.0), (edgeBoundMix * boundAcc)/(colAcc.a+(edgeBoundMix * boundAcc)) );
		colAcc.a = max(colAcc.a, boundAcc);
	}
	colAcc.a *= backAlpha;
	if ( overlays< 1 ) {
		FragColor = colAcc;
		return;
	}
	//overlay pass
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	prevGrad = vec4(0.0,0.0,0.0,0.0);
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
	while (samplePos.a <= len) {
		colorSample = texture3D(intensityOverlay,samplePos.xyz);
		if (colorSample.a > 0.00) {
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			colorSample.a *=  overlayFuzzy;
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
			dx = colAcc.a * pow(dx, overlayDepth);
			overMix *= 1.0 - dx;
		}
		colAcc.rgb = mix(colAcc.rgb, overAcc.rgb, overMix);
		colAcc.a = max(colAcc.a, overAcc.a);
	//}
    FragColor = colAcc;
}