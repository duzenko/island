#version 330 core

uniform mat4 modelMatrix, viewProjectionMatrix, shadowMatrix;
//uniform sampler2D heights;
//uniform float worldSize, visibility;

in vec3 vpos, vnorm;
in vec2 vtex;

out vec4 spos;
out vec3 fnorm;
out vec2 ftex;

void main() {
  vec4 fpos;
    ftex = vtex;
    fnorm = normalize(modelMatrix*vec4(vnorm, 0)).xyz;
    vec4 wpos = modelMatrix*vec4(vpos, 1);
    float height=0;//texture(heights, vec2(0.5, 0.5)+wpos.xy/worldSize).r*(-255)+255;
    fpos = viewProjectionMatrix*(wpos+vec4(0, 0, height, 0));
    spos = shadowMatrix*modelMatrix*vec4(vpos, 1);
  gl_Position = fpos;
}
