//pref
brighten|float|0.5|2|3.5
surfaceColor|float|0.0|1.0|1.0
overlayFuzzy|float|0.01|0.5|1
overlayDepth|float|0.0|8.0|64.0|Search depth to detect overlays
overlayOpacity|float|0.0|0.8|1.0|Translucency of overlays
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
uniform float brighten = 1.5;
uniform float surfaceColor = 1.0;
uniform float overlayDepth = 3.0;
uniform float overlayFuzzy = 0.5;
uniform float overlayOpacity = 0.8;
uniform int overlays = 0;
uniform float backAlpha = 0.5;
uniform mat3 NormalMatrix;

uniform sampler2D matcap2D;
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
	float ambient = 1.0;
	float diffuse = 0.3;
	float specular = 0.25;
	float shininess = 10.0;
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
	float stepSizeX2 = samplePos.a + (stepSize * 2.0);
	//fast pass - optional
	vec4 clipPos = samplePos;
	deltaDir = vec4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));
	while (samplePos.a <= len) {
		if ((texture(intensityVol,samplePos.xyz).a) > 0.0) break;
		samplePos += deltaDir;
	}
	if (samplePos.a > len) { //no hit: quit here
		//colAcc = vec4(1.0, 0.0, 0.0, 1.0);
		FragColor = colAcc;
		return;		
	}
	samplePos -= deltaDir;
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	deltaDir = vec4(dir.xyz * stepSize, stepSize);
	//end fastpass - optional
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	int isSurface = 0;
	if ( overlays < 1 ) isSurface = 1;
	vec4 overSample = vec4(0.0,0.0,0.0,0.0);
	while (samplePos.a <= len) {
		colorSample = texture(intensityVol,samplePos.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
		if (colorSample.a > 0.01) {
			bgNearest = min(samplePos.a,bgNearest);
			if (samplePos.a > stepSizeX2) {
				vec3 a = colorSample.rgb * ambient;
				gradSample= texture(gradientVol,samplePos.xyz);
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				vec3 n = normalize(normalize(NormalMatrix * gradSample.rgb));
				vec2 uv = n.xy * 0.5 + 0.5;
				vec3 d = texture(matcap2D,uv.xy).rgb;
				vec3 surf = mix(defaultDiffuse, a, surfaceColor); //0.67 as default Brighten is 1.5
				colorSample.rgb = d * surf * brighten;
			} else
				colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			if (isSurface == 0) {
				vec4 overPos = vec4(samplePos.xyz, 0.0);
				float overlaySearchDepth = overlayDepth * sliceSize;
				float maxA = 0.0;
				while (overPos.a <= overlaySearchDepth) {
					vec4 overSam = texture(intensityOverlay,overPos.xyz);
					overPos += deltaDir;
					if (overSam.a > 0.0) {
						maxA = max(maxA, overSam.a);
						overSample = overSample + overSam;
						isSurface += 1;
					}
				}
				if (isSurface > 0) {
					overSample = overSample/isSurface;
					overSample.a = maxA * overlayOpacity;
				}
				isSurface += 1;
			}
			colorSample.rgb = mix(colorSample.rgb, overSample.rgb, overSample.a);
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			if ( colAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	colAcc.a = colAcc.a/0.95;
	colAcc.a *= backAlpha;
    FragColor = colAcc;
}