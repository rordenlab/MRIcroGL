//vert
#version 120
uniform vec2 ViewportSize;
varying vec4 texCoord;
void main() {
    vec2 pixelPosition = gl_Vertex.xy;
    pixelPosition -= (ViewportSize/2.0);
    gl_Position = vec4((pixelPosition / (ViewportSize/2.0)), 0.0, 1.0);
    texCoord = gl_Color;
}
//frag
#version 120
varying vec4 texCoord;
uniform float backAlpha = 1.0;
uniform sampler3D tex, drawTex, overlay;
uniform sampler1D drawLUT;
uniform float drawAlpha = 0.5;
uniform int overlays = 0;
uniform vec3 texSize;
void main() {
	vec4 color = texture3D(tex,texCoord.xyz);
    color.a = smoothstep(0.0, 0.1, color.a);
    color.a *= backAlpha;
    //vec3 fx = normalize(fract(texCoord.xyz * texSize)) - 0.0;
    vec3 voxCen = fract(texCoord.xyz * texSize) - 0.5; // 0,0 for voxel center
    vec3 fx = normalize(fract(texCoord.xyz * texSize) - 0.5) - 0.0;
    //color.rgb = fx; 
    const float dPI_2 = 1.57079632679489661923;
	//const float dPI = 1.0/3.1415926535897932384626433832795;
    const float dPI = 1.0/dPI_2;
    
    //color.a = (acos(dot(fx, normalize(color.rgb))))* dPI;
    float dir = abs((acos(dot(normalize(color.rgb),fx))) -dPI_2) * dPI;
    dir = 1.0 - dir;
    //float dx = max(1.0 - length(voxCen), 0.0);
    //dir = step(0.9, dir);
    float dx = max(0.9 - length(voxCen), 0.0);
    float dx2 = dx * dx;
    dir = 1.0 - smoothstep(dx2 - 0.05, dx2, dir);
    //dir = smoothstep(dx - 0.05, dx, dir);
    //dir = smoothstep(dx-0.05,dx, dir);
    //dx = 1.0 - dx;
    //dir = smoothstep(dx,dx, dir);
    dx = smoothstep(0.65, 0.70, dx);
    //dir = max(dx, dir);
    //dx = sqrt(dx);
    //dx = step(0.5, dx);
    //color.a = mix(0.0,dx,  0.2 * dx);
    //color.a = dir;
    color.a = max(dir, dx);
    //color.a = dx;
    gl_FragColor = color;
    return;
    //color.r *= fract(texCoord.x * (texSize.x));
    //color.g *= fract(texCoord.y * (texSize.y));
    //color.b *= fract(texCoord.z * (texSize.z));
    
    

    //if ((overlays == 0) && (drawAlpha == 0.0)) color.r = 1.0; //test case where neither overlay or draw is visible
    if ((overlays == 0) && (drawAlpha == 0.0)) {
        gl_FragColor = color;
        return;
    }
    //if (color.a > 0.0) color.a = backAlpha;
    vec4 ocolor = texture3D(overlay, texCoord.xyz);
    color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);
    color.a = max(color.a, ocolor.a);
    if (drawAlpha <= 0.0) {
        gl_FragColor = color;
        return;
    }
    ocolor.a *= drawAlpha;
    color.rgb = mix(color.rgb, ocolor.rgb, ocolor.a);
    color.a = max(color.a, ocolor.a);
    gl_FragColor = color;
}  
