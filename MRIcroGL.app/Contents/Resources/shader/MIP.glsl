//pref
overAlpha|float|0.0|0.8|2.0|Ability to see overlay images added to background
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
uniform float stepSize, sliceSize;
uniform sampler3D intensityVol, gradientVol;
uniform sampler3D intensityOverlay, gradientOverlay;
uniform vec3 lightPosition, rayDir;
uniform vec4 clipPlane;
uniform float ambient = 1.0;
uniform float diffuse = 0.3;
uniform float specular = 0.25;
uniform float shininess = 10.0;
uniform float overlayDepth = 0.3;
uniform float overlayFuzzy = 0.5;
uniform  float overAlpha = 0.8;
uniform int overlays = 0;
uniform float backAlpha = 0.5;
uniform float overlayClip = 0.0;

vec3 GetBackPosition (vec3 startPosition) { //when does ray exit unit cube http://prideout.net/blog/?p=64
	vec3 invR = 1.0 / rayDir;
    vec3 tbot = invR * (vec3(0.0)-startPosition);
    vec3 ttop = invR * (vec3(1.0)-startPosition);
    vec3 tmax = max(ttop, tbot);
    vec2 t = min(tmax.xx, tmax.yz);
	return startPosition + (rayDir * min(t.x, t.y));
}
		
void main() {
    vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	vec3 dir = backPosition - start;
	float len = length(dir);
	dir = normalize(dir);
	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);
	vec4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	float overFarthest = len;
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	vec4 samplePos;
	//background pass
	samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);
	if (clipPlane.a > -0.5) {
		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);
		float dis = dot(dir,clipPlane.xyz);
		if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;
		//test: "return" fails on 2006MacBookPro10.4ATI1900, "discard" fails on MacPro10.5NV8800
		if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0))) {
			samplePos.a = len + 1.0;//no background
		} else if ((dis > 0.0) && (dis < len)) {
			if (frontface) {
				samplePos.a = dis;
				samplePos.xyz += dir * dis;
			} else {
				backPosition =  start + dir * (dis);
				len = length(backPosition - start);
			}
		}
	}
	vec4 clipPos = samplePos;
	float stepSizeX2 = samplePos.a + (stepSize * 2.0);
	//fast pass - optional
	deltaDir = vec4(dir.xyz * max(stepSize, sliceSize), max(stepSize, sliceSize));
	while (samplePos.a <= len) {
		if ((texture(intensityVol,samplePos.xyz).a) > 0.0) break;
		samplePos += deltaDir;
	}
	if ((samplePos.a > len) && ( overlays < 1 )) { //no hit: quit here
		FragColor = colAcc;
		return;		
	}
	samplePos -= deltaDir;
	deltaDir = vec4(dir.xyz * stepSize, stepSize);
	//end fastpass - optional
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	vec4 gradMax = colAcc;
	if ( overlays < 1 ) { //pass without overlays
		while (samplePos.a <= len) {
			colorSample = texture(intensityVol,samplePos.xyz);
			if (colorSample.a > colAcc.a)
				colAcc = colorSample;
			samplePos += deltaDir;
		} //while samplePos.a < len
		FragColor = colAcc;
		return;
	}
	//overlay pass
	vec4 ocolAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevNorm = ocolAcc;
	while (samplePos.a <= len) {
			colorSample = texture(intensityVol,samplePos.xyz);
			if (colorSample.a > colAcc.a)
				colAcc = colorSample;
			gradSample= texture(gradientVol,samplePos.xyz);
			if (gradSample.a > gradMax.a)
				gradMax = gradSample;
			//overlay:	
			colorSample.rgba = texture(intensityOverlay,samplePos.xyz);
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
			if (colorSample.a > 0.0) {
				gradSample = texture(gradientOverlay,samplePos.xyz);
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				if (gradSample.a < prevNorm.a)
					gradSample.rgb = prevNorm.rgb;
				prevNorm = gradSample;
				float lightNormDot = dot(gradSample.rgb, lightPosition);
				vec3 a = colorSample.rgb * ambient;
				vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
				float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
				colorSample.rgb = a + d + s;
			}
			colorSample.rgb *= colorSample.a;
			ocolAcc= (1.0 - ocolAcc.a) * colorSample + ocolAcc;
			samplePos += deltaDir;
	} //while samplePos.a < len
	if (ocolAcc.a > 0.01)  {
		ocolAcc.a = ocolAcc.a * (overAlpha);
		colAcc.rgb=mix(colAcc.rgb, ocolAcc.rgb,  ocolAcc.a);
		colAcc.a=max(colAcc.a,ocolAcc.a);
	}	
	FragColor = colAcc;
}