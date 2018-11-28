//pref
ambient|float|0.0|1.0|1
diffuse|float|0.0|0.3|1
specular|float|0.0|0.25|1
shininess|float|0.01|10.0|30
boundThresh|float|0.0|0.5|0.95
edgeBoundMix|float|0|0|1
overDistance|float|0.0|0.3|1
overAlpha|float|0.0|1.6|2.0
overShade|float|0.0|0.3|1.0
//vert
#version 330 core
layout(location = 0) in vec3 vPos;
out vec3 TexCoord1;
out vec4 vPosition;
uniform mat4 ModelViewProjectionMatrix;
void main() {
  TexCoord1 = vPos;
  gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);
  vPosition = gl_Position;
}
//frag
#version 330 core
in vec3 TexCoord1;
out vec4 FragColor;
in vec4 vPosition;
uniform int loops;
uniform float stepSize, sliceSize;
uniform sampler3D intensityVol, gradientVol;
uniform sampler3D intensityOverlay, gradientOverlay;
uniform vec3 lightPosition, rayDir;
uniform vec4 clipPlane;
uniform float edgeBoundMix = 0.0;
uniform float overDistance = 0.3;
uniform float ambient = 1.0;
uniform float diffuse = 0.3;
uniform float boundThresh = 0.3;
uniform float specular = 0.25;
uniform float shininess = 10.0;
uniform float overAlpha = 0.8;
uniform float backAlpha = 0.5;
uniform float overShade = 0.3;
uniform int overlays = 0;
vec3 GetBackPosition (vec3 startPosition) { //when does ray exit unit cube http://prideout.net/blog/?p=64
	vec3 invR = 1.0 / rayDir;
    vec3 tbot = invR * (vec3(0.0)-startPosition);
    vec3 ttop = invR * (vec3(1.0)-startPosition);
    vec3 tmax = max(ttop, tbot);
    vec2 t = min(tmax.xx, tmax.yz);
	return startPosition + (rayDir * min(t.x, t.y));
}
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
	float stepSizex2 = -1.0;
	if (clipPlane.a > -0.5) {
		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);
		float dis = dot(dir,clipPlane.xyz);
		if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;
		//test: "return" fails on 2006MacBookPro10.4ATI1900, "discard" fails on MacPro10.5NV8800
		//if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0))) {
		// FragColor = vec4(0.0,0.0,0.0,0.0);
		// return;
		//}
		//if ((dis > 0.0) && (dis < len)) {
			if (frontface)
				clipStart = dis;
			else
				clipEnd =  dis;
		//}
		stepSizex2 = clipStart + (stepSize * 2);
	}
	vec3 deltaDir = dir * stepSize;
	vec4 ocolAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 ocolorSample, colorSample, gradientSample, colAcc = vec4(0.0,0.0,0.0,0.0);
	float lengthAcc = 0.0;
	float boundAcc = 0.0;
	float boundAcc2 = 0.0;
	int overDepth = 0;
	int backDepthEnd, backDepthStart = loops;
	vec3 lightDirHeadOn = rayDir.xyz;
	float edgeThresh = 0.01;
	float edgeExp = 0.5;
	float diffuseDiv = diffuse / 4.0;
	vec3 samplePos = start.xyz + deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));
	vec4 prevNorm = vec4(0.0,0.0,0.0,0.0);
	vec4 oprevNorm = vec4(0.0,0.0,0.0,0.0);
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	float opacityCorrection = stepSize/sliceSize;
	float alphaTerminate = 0.95;
	if ( overlays > 0 ) alphaTerminate = 2.0; //impossible value: no early termination with overlays
	for(int i = 0; i < loops; i++) {
		if ((lengthAcc <= clipStart) || (lengthAcc > clipEnd)) {
			colorSample.a = 0.0;
		} else {
			colorSample = texture(intensityVol,samplePos);
			if ((lengthAcc <= stepSizex2) && (colorSample.a > 0.01) )  colorSample.a = sqrt(colorSample.a); //opaque clipping surface
			colorSample.a = 1.0-pow((1.0 - colorSample.a), opacityCorrection);
			if ((colorSample.a > 0.01) && (lengthAcc > stepSizex2)  ) {
				if (backDepthStart == loops) backDepthStart = i;
				backDepthEnd = i;
				gradientSample= texture(gradientOverlay,samplePos); //interpolate gradient direction and magnitude
				//gradientSample.rgb = normalize(gradientSample.rgb*2.0 - 1.0); //direction saved as 0..1, rescale to -1..1
				gradientSample.rgb = gradientSample.rgb*2.0 - 1.0; //do not normalize: vec3(0,0,0)!!
				//re-use previous normal if it has larger magnitude
				if (gradientSample.a < prevNorm.a)
					gradientSample.rgb = prevNorm.rgb;
				prevNorm = gradientSample;


				//Edge shading - darken edges parallel with viewing direction
				float lightNormDot = dot(gradientSample.rgb, lightDirHeadOn); //with respect to viewer
				float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradientSample.a,0.3);
				if (edgeVal >= edgeThresh)
					colorSample.rgb = mix(colorSample.rgb, vec3(0.0,0.0,0.0), pow((edgeVal-edgeThresh)/(1.0-edgeThresh),4.0));
				//specular
				lightNormDot = dot(gradientSample.rgb, lightPosition); //with respect to light location

				if (lightNormDot > 0.0) {
					colorSample.rgb += (lightNormDot * diffuse) - diffuseDiv;
					colorSample.rgb +=   specular * pow(max(dot(reflect(lightPosition, gradientSample.rgb), dir), 0.0), shininess);
				} else
					colorSample.rgb -= diffuseDiv;
				if (gradientSample.a > boundThresh) {
					float lightNormDot = dot(gradientSample.rgb, lightDirHeadOn); //with respect to viewer
					float boundAlpha = pow(1.0-abs(lightNormDot),6.0);
					boundAlpha = 1.0-pow((1.0 - boundAlpha), opacityCorrection);
					boundAcc += (1.0 - boundAcc2) * boundAlpha;
					boundAcc2 += (1.0 - boundAcc2) * boundAlpha;
				}
			};
		}
		if ( overlays > 0 ) {
			gradientSample= texture(gradientOverlay,samplePos); //interpolate gradient direction and magnitude
			if (gradientSample.a > 0.01) {
				if (gradientSample.a < oprevNorm.a)
					gradientSample.rgb = oprevNorm.rgb;
				oprevNorm = gradientSample;
				gradientSample.rgb = normalize(gradientSample.rgb*2.0 - 1.0); //direction saved as 0..1, rescale to -1..1
				ocolorSample = texture(intensityOverlay,samplePos);
				ocolorSample.a *= gradientSample.a; //modulate by gradient
				ocolorSample.a = sqrt(ocolorSample.a);
				//Edge shading - darken edges parallel with viewing direction
				float lightNormDot = dot(gradientSample.rgb, lightDirHeadOn); //with respect to viewer
				float edgeVal = pow(1.0-abs(lightNormDot),edgeExp) * pow(gradientSample.a,overShade);
				ocolorSample.a = pow(ocolorSample.a, 1.0 -edgeVal);
				ocolorSample.rgb = mix(ocolorSample.rgb, vec3(0.0,0.0,0.0), edgeVal);
				//if (edgeVal >= edgeThresh)
				//	ocolorSample.rgb = mix(ocolorSample.rgb, vec3(0.0,0.0,0.0), pow((edgeVal-edgeThresh)/(1.0-edgeThresh),4.0));
				//specular
				lightNormDot = dot(gradientSample.rgb, lightPosition); //with respect to light location
				if (lightNormDot > 0.0)
					ocolorSample.rgb +=   overLight * specular * pow(max(dot(reflect(lightPosition, gradientSample.rgb), dir), 0.0), shininess);
				//float lightAmount = 0.1 * overLight;
				//diffuse
		  		//if (lightAmount > 0.0)
				//	ocolorSample.rgb += lightAmount*dot(normalize(lightPosition), gradientSample.rgb);
				//Diffuse lighting
				//float diffTerm =  max(0.5 * lightNormDot + 0.5, 0.5);
				//Quadratic falloff of the diffusive term
				//diffTerm *= diffTerm;
				//ocolorSample.rgb *= diffuse* (diffTerm) + (1.0 - diffuse);
				ocolorSample.a *= overAlphaFrac;
				if ( ocolorSample.a > 0.2) {
					if (overDepth == 0) overDepth = i;
					float overRatio = colorSample.a/(ocolorSample.a);
					if (colorSample.a > 0.02)
						colorSample.rgb = mix( colorSample.rgb, ocolorSample.rgb, overRatio);
					else
						colorSample.rgb = ocolorSample.rgb;
					colorSample.a = max(ocolorSample.a, colorSample.a);
				}
				//xxx colorSample= max(ocolorSample, colorSample);
				//accumulate overlay color
				ocolorSample.a = 1.0-pow((1.0 - ocolorSample.a), opacityCorrection);
				overAcc= (1.0 - overAcc.a) * ocolorSample + overAcc;
				boundAcc2 += (1.0 - boundAcc2) * ocolorSample.a;
			}
		}

		colorSample.rgb *= colorSample.a;
		//accumulate color
		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
		samplePos += deltaDir;
		lengthAcc += stepSize;
		if ( lengthAcc >= len || colAcc.a > alphaTerminate )
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