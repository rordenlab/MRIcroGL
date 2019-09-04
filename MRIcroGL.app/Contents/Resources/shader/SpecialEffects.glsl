//pref
ambient|float|0.0|1.0|1|Illuminate surface regardless of lighting
diffuse|float|0.0|0.2|1|Illuminate surface based on light position
specular|float|0.0|0.2|1|Glint from shiny surfaces
shininess|float|0.01|10.0|30|Specular reflections can be rough or precise
overlayFuzzy|float|0.01|0.5|1|Do overlay layers have blurry surfaces?
overlayDepth|float|0.0|0.3|0.8|Can we see overlay layers deep beneath the background image?
overlayClip|float|0|0|1|Does clipping also influence overlay layers?
showGradient|float|0|0|1|Display surface angle
doPoor|float|0|0|1|Poor quality reveals rendering strategy
doJitter|float|0|1|1|Jitter hides wood-grain artifacts
//vert

//pref
ambient|float|0.0|1.0|1|Illuminate surface regardless of lighting
diffuse|float|0.0|0.2|1|Illuminate surface based on light position
specular|float|0.0|0.2|1|Glint from shiny surfaces
shininess|float|0.01|10.0|30|Specular reflections can be rough or precise
overlayFuzzy|float|0.01|0.5|1|Do overlay layers have blurry surfaces?
overlayDepth|float|0.0|0.3|0.8|Can we see overlay layers deep beneath the background image?
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
uniform int overlays = 0;
uniform float backAlpha = 0.5;
uniform float overlayClip = 0.0;
uniform float doPoor = 0.0;
uniform float doJitter = 0.0;
uniform float showGradient = 0.0;

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
	vec4 samplePos;
	//background pass
	samplePos = vec4(start.xyz +deltaDir.xyz*ran, 0.0);
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
	float stepSizeX2 = samplePos.a + (stepSizeX * 2.0);
	//fast pass - optional
	deltaDir = vec4(dir.xyz * max(stepSizeX, sliceSize), max(stepSizeX, sliceSize));
	while (samplePos.a <= len) {
		if ((texture(intensityVol,samplePos.xyz).a) > 0.0) break;
		samplePos += deltaDir;
	}
	if ((samplePos.a > len) && ( overlays < 1 )) { //no hit: quit here
		//colAcc = vec4(1.0, 0.0, 0.0, 1.0);
		FragColor = colAcc;
		return;		
	}
	samplePos -= deltaDir;
	deltaDir = vec4(dir.xyz * stepSizeX, stepSizeX);
	//end fastpass - optional
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	while (samplePos.a <= len) {
		colorSample = texture(intensityVol,samplePos.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSizeX/sliceSize);
		if (colorSample.a > 0.0) {
			if (showGradient > 0.5) {
				colorSample.rgb = abs(texture(gradientVol,samplePos.xyz).rgb);
				colorSample.rgb = colorSample.rgb*2.0 - 1.0;
				colorSample.rgb = abs(colorSample.rgb);
			}
			bgNearest = min(samplePos.a,bgNearest);
			if (samplePos.a > stepSizeX2) {
				gradSample= texture(gradientVol,samplePos.xyz);
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				vec3 a = colorSample.rgb * ambient;
				float lightNormDot = dot(gradSample.rgb, lightPosition);
				vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
				float s =   specular * pow(max(dot(reflect(lightPosition, gradSample.rgb), dir), 0.0), shininess);
				colorSample.rgb = a + d + s;

			} else {
				colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			}
			colorSample.rgb *= colorSample.a;
			colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
			if ( colAcc.a > 0.95 )
				break;
		}
		samplePos += deltaDir;
	} //while samplePos.a < len
	colAcc.a = colAcc.a/0.95;
	if ( overlays < 1 ) {
		FragColor = colAcc;
		return;
	}
	//overlay pass
	vec4 overAcc = vec4(0.0,0.0,0.0,0.0);
	prevGrad = vec4(0.0,0.0,0.0,0.0);
	if (overlayClip > 0)
		samplePos = clipPos;
	else
		samplePos = vec4(start.xyz +deltaDir.xyz* ran, 0.0);
	//fast pass - optional
	deltaDir = vec4(dir.xyz * max(stepSizeX, sliceSize), max(stepSizeX, sliceSize));
	while (samplePos.a <= len) {
		if ((texture(intensityOverlay,samplePos.xyz).a) > 0.0) break;
		samplePos += deltaDir;
	}
	samplePos -= deltaDir;
	deltaDir = vec4(dir.xyz * stepSizeX, stepSizeX);
	//end fastpass - optional
	while (samplePos.a <= len) {
		colorSample = texture(intensityOverlay,samplePos.xyz);
		if (colorSample.a > 0.00) {
			colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSizeX/sliceSize);
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