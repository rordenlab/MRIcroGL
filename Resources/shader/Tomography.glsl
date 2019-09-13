//pref
brighten|float|0.5|2|3.5
surfaceColor|float|0.0|1.0|1.0
overlayFuzzy|float|0.01|0.5|1
overlayDepth|float|0.0|0.15|0.99
gradientMix|float|0.0|1|1
intensityMix|float|0.0|1|1
surfaceHardness|float|0.0|0.75|1
overlayClip|float|0|0|1|Does clipping also influence overlay layers?
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
uniform float overlayDepth = 0.3;
uniform float overlayFuzzy = 0.5;
uniform int overlays = 0;
uniform float backAlpha = 0.5;
uniform float gradientMix = 0.5;
uniform float intensityMix = 0.5;
uniform float surfaceHardness = 0.5;
uniform float overlayClip = 0.0;
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
	//background pass
	float noClipLen = len;
	vec4 samplePos = vec4(start.xyz, 0.0);
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
	//fast pass - optional
	deltaDir = vec4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));
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
	vec4 colorMax = vec4(0.0, 0.0, 0.0, 0.0);
	if (samplePos.a < clipPos.a) {
		samplePos = clipPos;
		float stepSizeX2 = samplePos.a + (stepSize * 2.0);
		while (samplePos.a <= stepSizeX2) {
			colorSample = texture(intensityVol,samplePos.xyz);
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
		colorSample = texture(intensityVol,samplePos.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
		if (colorSample.a > 0.01) {
			if (colorSample.a > colorMax.a) colorMax = colorSample;
			bgNearest = min(samplePos.a,bgNearest);
				vec3 a = colorSample.rgb * ambient;
				gradSample= texture(gradientVol,samplePos.xyz);
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				if (gradSample.a > gradMax.a)
					gradMax = gradSample;
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				gradAcc = (1.0 - gradAcc.a) * prevGrad + gradAcc;
				vec3 n = normalize(normalize(NormalMatrix * gradSample.rgb));
				vec2 uv = n.xy * 0.5 + 0.5;
				vec3 d = texture(matcap2D,uv.xy).rgb;
				vec3 surf = mix(defaultDiffuse, a, surfaceColor); //0.67 as default Brighten is 1.5
				colorSample.rgb = d * surf * brighten;
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			if ( colAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	
	//start tomography
	float alphaThresh = 0.95;
	gradSample = mix(gradAcc, gradMax, gradientMix);
	if ((samplePos.a < len) && (surfaceHardness > 0.0) && (gradSample.a > 0.001)) {
		colorSample = texture(intensityVol,samplePos.xyz);
		colorSample = mix(colorSample, colorMax, intensityMix);
		vec3 n = normalize(normalize(NormalMatrix * gradSample.rgb));
		vec2 uv = n.xy * 0.5 + 0.5;
		vec3 d = texture(matcap2D,uv.xy).rgb;
		vec3 surf = mix(defaultDiffuse, colorSample.rgb, surfaceColor); //0.67 as default Brighten is 1.5
		colorSample.rgb = d * surf * (brighten / 2.0);
		colAcc.rgb = mix(colAcc.rgb, colorSample.rgb,  surfaceHardness);		
	} else {
		if (colAcc.a < alphaThresh) alphaThresh = 0.0;  
		colAcc.a = mix(colAcc.a, alphaThresh,  surfaceHardness);		
	}
	//end tomography
	colAcc.a = colAcc.a/0.95;
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
		samplePos = vec4(start.xyz +deltaDir.xyz* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453)), 0.0);
	}
	//fast pass - optional
	clipPos = samplePos;
	deltaDir = vec4(dir.xyz * max(stepSize, sliceSize * 1.95), max(stepSize, sliceSize * 1.95));
	while (samplePos.a <= len) {
		if ((texture(intensityOverlay,samplePos.xyz).a) > 0.0) break;
		samplePos += deltaDir;
	}
	samplePos -= deltaDir;
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	deltaDir = vec4(dir.xyz * stepSize, stepSize);
	//end fastpass - optional
	while (samplePos.a <= len) {
		colorSample = texture(intensityOverlay,samplePos.xyz);
		if (colorSample.a > 0.00) {
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
			colorSample.a *=  overlayFuzzy;
			vec3 a = colorSample.rgb * ambient;
			float s =  0;
			vec3 d = vec3(0.0, 0.0, 0.0);
			overFarthest = samplePos.a;
			//gradient based lighting http://www.mccauslandcenter.sc.edu/mricrogl/gradients
			gradSample = texture(gradientOverlay,samplePos.xyz); //interpolate gradient direction and magnitude
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
	colAcc.a *= backAlpha;
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