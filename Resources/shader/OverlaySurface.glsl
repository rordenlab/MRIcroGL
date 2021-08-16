//pref
brighten|float|0.5|2|3.5
surfaceColor|float|0.0|1.0|1.0
overlayFuzzy|float|0.01|0.5|1
overlayDepth|float|0.0|8.0|64.0|Search depth to detect overlays
overlayOpacity|float|0.0|0.8|1.0|Translucency of overlays
//frag

uniform float brighten = 1.5;
uniform float surfaceColor = 1.0;
uniform float overlayFuzzy = 0.5;
uniform float overlayDepth = 3.0;
uniform float overlayOpacity = 0.8;

uniform mat3 NormalMatrix;
uniform sampler2D matcap2D;

void main() {
	vec3 start = TexCoord1.xyz;
	vec3 backPosition = GetBackPosition(start);
	vec3 dir = backPosition - start;
	float len = length(dir);
	dir = normalize(dir);
	gl_FragDepth = 1.0;
	int nHit = 0;	
	vec4 deltaDir = vec4(dir.xyz * stepSize, stepSize);
	vec4 gradSample, colorSample;
	float bgNearest = len; //assume no hit
	float ambient = 1.0;
	float diffuse = 0.3;
	float specular = 0.25;
	float shininess = 10.0;
	vec4 colAcc = vec4(0.0,0.0,0.0,0.0);
	vec4 prevGrad = vec4(0.0,0.0,0.0,0.0);
	//background pass
	vec4 samplePos = vec4(start.xyz, 0.0);
	vec4 clipPos = applyClip(dir, samplePos, len);
	float stepSizeX2 = samplePos.a + (stepSize * 2.0);
	//fast pass - optional
	fastPass (len, dir, intensityVol, samplePos);
	#if ( __VERSION__ > 300 )
	if ((samplePos.a > len) && ( overlays < 1 )) { //no hit
		FragColor = colAcc;
		return;
	}
	#else
	if ((textureSz.x < 1) || ((samplePos.a > len) && ( overlays < 1 ))) { //no hit
		gl_FragColor = colAcc;
		return;		
	}	
	#endif
	if (samplePos.a < clipPos.a)
		samplePos = clipPos;
	//end fastpass - optional
	float ran = fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453);
	samplePos += deltaDir * ran;
	vec3 defaultDiffuse = vec3(0.5, 0.5, 0.5);
	int isSurface = 0;
	if ( overlays < 1 ) isSurface = 1;
	vec4 overSample = vec4(0.0,0.0,0.0,0.0);
	while (samplePos.a <= len) {
		colorSample = texture3Df(intensityVol, samplePos.xyz);
		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);
		if (colorSample.a > 0.01) {
			if (nHit < 1) {
				nHit ++;
				bgNearest = samplePos.a;
				setDepthBuffer(samplePos.xyz);
			}
			if (samplePos.a > stepSizeX2) {
				vec3 a = colorSample.rgb * ambient;
				gradSample= texture3Df(gradientVol,samplePos.xyz);
				gradSample.rgb = normalize(gradSample.rgb*2.0 - 1.0);
				//reusing Normals http://www.marcusbannerman.co.uk/articles/VolumeRendering.html
				if (gradSample.a < prevGrad.a)
					gradSample.rgb = prevGrad.rgb;
				prevGrad = gradSample;
				vec3 n = normalize(NormalMatrix * gradSample.rgb);
				vec2 uv = n.xy * 0.5 + 0.5;
				vec3 d = texture2D(matcap2D,uv.xy).rgb;
				vec3 surf = mix(defaultDiffuse, a, surfaceColor); //0.67 as default Brighten is 1.5
				colorSample.rgb = d * surf * brighten;
			} else
				colorSample.a = clamp(colorSample.a*3.0,0.0, 1.0);
			if (isSurface == 0) {
				vec4 overPos = vec4(samplePos.xyz, 0.0);
				float overlaySearchDepth = overlayDepth * sliceSize;
				float maxA = 0.0;
				while (overPos.a <= overlaySearchDepth) {
					vec4 overSam = texture3Df(intensityOverlay, overPos.xyz);
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
	#if ( __VERSION__ > 300 )
	FragColor = colAcc;
	#else
	gl_FragColor = colAcc;
	#endif
}