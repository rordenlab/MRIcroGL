//pref
ambient|float|0.0|1.0|1|Illuminate surface regardless of lighting
diffuse|float|0.0|0.3|1|Illuminate surface based on light position
specular|float|0.0|0.25|1|Glint from shiny surfaces
shininess|float|0.01|10.0|30|Specular reflections can be rough or precise
overDistance|float|0.0|0.3|1|Ability to see overlays beneath the background surface
overAlpha|float|0.0|0.8|1.0|Ability to see overlay images added to background
overShade|float|0.0|0.3|1.0|Control lighting applied to overlays
//frag

#line 13

uniform float ambient = 1.0;
uniform float diffuse = 0.3;
uniform float specular = 0.25;
uniform float shininess = 10.0;
uniform float overDistance = 0.3;
uniform float overAlpha = 0.8;
uniform float overShade = 0.3;

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
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	float noClipLen = len;
	vec4 samplePos = vec4(start.xyz, 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
	float opacityCorrection = stepSize/sliceSize;
	
		float stepSizeX2 = samplePos.a + (stepSize * 2.0);
		while (samplePos.a <= stepSizeX2) {
			colorSample = texture3D(intensityVol,samplePos.xyz);
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			samplePos += deltaDir;
		}
	//colAcc = vec4(0.0,0.0,0.0,0.0);
	float backStart = clipPos.a;
	float backLen = len;
	samplePos = vec4(start.xyz, 0.0)+= deltaDir * ran;
	len = noClipLen;
	
	vec4 ocolAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 ocolorSample;
	float overDepth = -1;
	float backDepthEnd, backDepthStart = 10;
	vec3 lightDirHeadOn = rayDir.xyz;
	float edgeThresh = 0.01;
	float edgeExp = 0.5;
	float diffuseDiv = diffuse / 4.0;
	vec4 oprevGrad = vec4(0.0,0.0,0.0,0.0);
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	colorSample = vec4(0.0,0.0,0.0,0.0);
	float alphaTerminate = 0.95;
	if ( overlays > 0 ) alphaTerminate = 2.0; //impossible value: no early termination with overlays
	float overAlphaFrac = overAlpha;
	//if (overAlphaFrac > 1.0) overAlphaFrac = 1.0;
	while (samplePos.a <= len) {
		
			
			if ((samplePos.a  >= backStart) && (samplePos.a <= backLen))  {
				
				colorSample = texture3D(intensityVol,samplePos.xyz);
				if (colorSample.a > 0.0) {
					colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
					backDepthStart = min(backDepthStart, samplePos.a);
					backDepthEnd = samplePos.a;
					gradSample= texture3D(gradientVol,samplePos.xyz);
					gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
					if (gradSample.a < prevGrad.a)
						gradSample.rgb = prevGrad.rgb;
					prevGrad = gradSample;
					//Edge shading - darken edges parallel with viewing direction
					float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
					float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradSample.a,0.3);
					if (edgeVal >= edgeThresh)
						colorSample.rgb = mix(colorSample.rgb, vec3(0.0,0.0,0.0), pow((edgeVal-edgeThresh)/(1.0-edgeThresh),4.0));
	
	
					lightNormDot = dot(gradSample.rgb, lightPosition);
					vec3 a = colorSample.rgb * ambient;
					vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
					float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
					colorSample.rgb = a + d + s;
					colorSample.rgb *= colorSample.a;
					colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
					if (colAcc.a > alphaTerminate )
						break;	
				}	
		}
		
		if ( overlays > 0 ) {
			vec4 ocolorSample = texture3D(intensityOverlay,samplePos.xyz);
			ocolorSample.a = 1.0-pow((1.0 - ocolorSample.a), opacityCorrection);
				
			if (ocolorSample.a > 0.01) {
				gradSample = texture3D(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				if (gradSample.a < oprevGrad.a)
					gradSample.rgb = oprevGrad.rgb;
				oprevGrad = gradSample;
				//overDepth = min(overDepth, samplePos.a);
				if (overDepth < 0) overDepth = samplePos.a;
				//Edge shading - darken edges parallel with viewing direction
				float lightNormDot = dot(gradSample.rgb, lightDirHeadOn); //with respect to viewer
				float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradSample.a,overShade);
				ocolorSample.a = pow(ocolorSample.a, 1.0 -edgeVal);
				ocolorSample.rgb = mix(ocolorSample.rgb, vec3(0.0,0.0,0.0), edgeVal);
				lightNormDot = dot(gradSample.rgb, lightPosition);
				vec3 a = ocolorSample.rgb * ambient;
				vec3 d = max(lightNormDot, 0.0) * ocolorSample.rgb * diffuse;
				float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
				ocolorSample.rgb = a + d + s;
				ocolorSample.a *= overAlphaFrac;
				//if ( ocolorSample.a > 0.2) {
					//float overRatio = colorSample.a/(ocolorSample.a);
					//if (colorSample.a > 0.02)
					//	colorSample.rgb = mix( colorSample.rgb, ocolorSample.rgb, overRatio);
					//else
					//	colorSample.rgb = ocolorSample.rgb;
					//colorSample.a = max(ocolorSample.a, colorSample.a);
					//colAcc= (1.0 - colAcc.a) * ocolorSample + colAcc;
					
				//}
				ocolorSample.a = 1.0-pow((1.0 - ocolorSample.a), opacityCorrection);
				overAcc= (1.0 - overAcc.a) * ocolorSample + overAcc;
			}
		}
		samplePos += deltaDir;


	}

	colAcc.a*=backAlpha;
	if ((overAcc.a > 0.01) && (overAlpha > 0.0))  {
		colAcc.a=max(colAcc.a,overAcc.a);
		if ( (overDistance > 0.0) && (overDepth > backDepthStart) && (backDepthEnd > backDepthStart)) {
			if (overDepth > backDepthEnd) overDepth = backDepthStart; // backDepthEnd
			float dx = float(overDepth-backDepthStart)/ float(backDepthEnd - backDepthStart);
			dx = pow(1.0-dx, overDistance);
			dx = pow(dx, 2.0);
			overAcc *= dx;
		}
		//overAlphaFrac = (overAlpha - 1.0);
		overAlphaFrac = overAcc.a * (overAlpha);
		if (overAcc.a > 0.0)
		colAcc.rgb=mix(colAcc.rgb, overAcc.rgb,  overAlphaFrac);
	}
	FragColor = colAcc;
}