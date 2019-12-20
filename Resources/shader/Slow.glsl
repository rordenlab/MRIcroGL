//pref
ambient|float|0.0|1.0|1|Illuminate surface regardless of lighting
diffuse|float|0.0|0.3|1|Illuminate surface based on light position
specular|float|0.0|0.25|1|Glint from shiny surfaces
shininess|float|0.01|10.0|30|Specular reflections can be rough or precise
boundThresh|float|0.0|0.5|0.95|Boundary threshold (requires high edgeBoundMix)
edgeBoundMix|float|0|0|1|Mixture of edge and boundary opacity
overDistance|float|0.0|0.3|1|Ability to see overlays beneath the background surface
overAlpha|float|0.0|1.6|2.0|Ability to see overlay images added to background
overShade|float|0.0|0.3|1.0|Control lighting applied to overlays
//frag
uniform float ambient = 1.0;
uniform float diffuse = 0.3;
uniform float specular = 0.25;
uniform float shininess = 10.0;
uniform float boundThresh = 0.3;
uniform float edgeBoundMix = 0.0;
uniform float overDistance = 0.3;
uniform float overAlpha = 0.8;
uniform float overShade = 0.3;

uniform int loops;

void main() {
	//FragColor = vec4(0.0, 1.0, 0.0, 1.0); return;
	vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	//FragColor = vec4(start, 1.0); return;
	//FragColor = vec4(backPosition, 1.0); return;
	vec3 dir = backPosition - start;
	//FragColor = vec4(dir, 1.0); return;
	float len = length(dir);
	dir = normalize(dir);
	float overAlphaFrac = overAlpha;
	if (overAlphaFrac > 1.0) overAlphaFrac = 1.0;
	float overLight = 0.5;//1.0;
	float clipStart = 0.0;
	float clipEnd = len;
	float stepSizeX2 = -stepSize;
	if (clipPlane.a > -0.5) {
		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);
		float dis = dot(dir,clipPlane.xyz);
		if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;
			if (frontface) {
				clipStart = dis;
				stepSizeX2 = clipStart + (stepSize * 2); //avoid specular effects in clip plane
			} else
				clipEnd =  dis;
		stepSizeX2 = clipStart + (stepSize * 2);
	}
	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);
	vec4 ocolAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 ocolorSample, colorSample, gradSample, colAcc = vec4(0.0,0.0,0.0,0.0);
	float boundAcc = 0.0;
	float boundAcc2 = 0.0;
	float overDepth = -1;
	float backDepthEnd, backDepthStart = -1;
	vec3 lightDirHeadOn = rayDir.xyz;
	float edgeThresh = 0.01;
	float edgeExp = 0.5;
	float diffuseDiv = diffuse / 4.0;
	vec4 samplePos = vec4(start.xyz + deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)),0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	vec4 oprevGrad = vec4(0.0,0.0,0.0,0.0);
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	float opacityCorrection = stepSize/sliceSize;
	float alphaTerminate = 0.95;
	if ( overlays > 0 ) alphaTerminate = 2.0; //impossible value: no early termination with overlays
	for(int i = 0; i < loops; i++) {
		colorSample = vec4(0.0,0.0,0.0,0.0);
		if ((samplePos.a > clipStart) && (samplePos.a < clipEnd)) {
			colorSample = texture3D(intensityVol,samplePos.xyz);
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			if ((colorSample.a > 0.01) && (samplePos.a > stepSizeX2)) {
				if (backDepthStart < 0) backDepthStart = samplePos.a;
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
		if ( overlays > 0 ) {
			vec4 ocolorSample = texture3D(intensityOverlay,samplePos.xyz);

			ocolorSample.a = 1.0-pow((1.0 - ocolorSample.a), opacityCorrection);
			if (ocolorSample.a > 0.01) {
				gradSample = texture3D(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude

				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				if (gradSample.a < oprevGrad.a)
					gradSample.rgb = oprevGrad.rgb;
				oprevGrad = gradSample;
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
				if ( ocolorSample.a > 0.2) {
					//if (overDepth == 0) overDepth = i;
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
		samplePos += deltaDir;
		if ( samplePos.a >= len || colAcc.a > alphaTerminate )
			break;
	}

	colAcc.a*=backAlpha;
	if ((edgeBoundMix > 0.0) && ((colAcc.a + boundAcc) > 0.0)) {
		colAcc.rgb = mix(colAcc.rgb, vec3(0.0,0.0,0.0), (edgeBoundMix * boundAcc)/(colAcc.a+(edgeBoundMix * boundAcc)) );
		colAcc.a = max(colAcc.a, boundAcc);
	}
	if ((overAcc.a > 0.01) && (overAlpha > 1.0))  {
		colAcc.a=max(colAcc.a,overAcc.a);
		if ( (overDistance > 0.0) && (overDepth > backDepthStart) && (backDepthEnd > backDepthStart)) {
			if (overDepth > backDepthEnd) overDepth = backDepthStart; // backDepthEnd
			float dx = float(overDepth-backDepthStart)/ float(backDepthEnd - backDepthStart);
			dx = pow(1.0-dx, overDistance);
			dx = pow(dx, 2.0);
			overAcc *= dx;
		}
		//overAlphaFrac = (overAlpha - 1.0);
		overAlphaFrac = overAcc.a * (overAlpha - 1.0);
		if (overAcc.a > 0.0)
		colAcc.rgb=mix(colAcc.rgb, overAcc.rgb,  overAlphaFrac);
	}
	FragColor = colAcc;
}