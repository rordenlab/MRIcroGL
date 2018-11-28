//pref
ambient|float|0.0|1.0|2
diffuse|float|0.0|0.3|2
specular|float|0.0|0.25|2
shininess|float|0.01|10.0|30
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
uniform vec3 lightPosition, rayDir;
uniform vec4 clipPlane;
uniform float ambient = 1.0;
uniform float diffuse = 0.3;
uniform float specular = 0.25;
uniform float shininess = 10.0;
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
	if (clipPlane.a > -0.5) {
		bool frontface = (dot(dir , clipPlane.xyz) > 0.0);
		float dis = dot(dir,clipPlane.xyz);
		if (dis != 0.0  )  dis = (-clipPlane.a - dot(clipPlane.xyz, start.xyz-0.5)) / dis;
		//test: "return" fails on 2006MacBookPro10.4ATI1900, "discard" fails on MacPro10.5NV8800
		if (((frontface) && (dis >= len)) || ((!frontface) && (dis <= 0.0))) {
		 FragColor = vec4(0.0,0.0,0.0,0.0);
		 return;
		}
		if ((dis > 0.0) && (dis < len)) {
			if (frontface)
				start = start + dir * dis;
			else
				backPosition =  start + dir * (dis);
			len = length(backPosition - start);
		}
	}
	vec3 deltaDir = dir * stepSize;
	vec4 colorSample,gradientSample,colAcc = vec4(0.0,0.0,0.0,0.0);
	float lengthAcc = 0.0;
	float stepSizeX2 = stepSize * 2;
	vec3 samplePos = start.xyz + deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));
	vec4 prevNorm = vec4(0.0,0.0,0.0,0.0);
	for(int i = 0; i < loops; i++) {
		colorSample.rgba = texture(intensityVol,samplePos);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
		if ((colorSample.a > 0.01) && (lengthAcc > stepSizeX2)) {
			gradientSample= texture(gradientVol,samplePos);
			gradientSample.rgb = normalize(gradientSample.rgb*2.0 - 1.0);
			if (gradientSample.a < prevNorm.a)
				gradientSample.rgb = prevNorm.rgb;
			prevNorm = gradientSample;
			float lightNormDot = dot(gradientSample.rgb, lightPosition);
			vec3 a = colorSample.rgb * ambient;
			vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;
			float s =   specular * pow(max(dot(reflect(lightPosition, gradientSample.rgb), dir), 0.0), shininess);
			colorSample.rgb = a + d + s;
		}
		colorSample.rgb *= colorSample.a;
		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;
		samplePos += deltaDir;
		lengthAcc += stepSize;
		if ( lengthAcc >= len || colAcc.a > 0.95 )
			break;
	}
	colAcc.a = colAcc.a/0.95;
	FragColor = colAcc;
}