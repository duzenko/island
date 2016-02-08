#version 330 core

uniform mat4 modelMatrix, viewProjectionMatrix, shadowMatrix;
uniform sampler2D heights;

uniform float terrainSize;

out vec4 spos, wpos;
out vec3 fnorm;
out vec2 ftex;

vec3 calcNormal(vec3 v, vec3 w) {
}

vec3

void main() {
    vec2 index = vec2((gl_VertexID-gl_VertexID%2)/2, gl_InstanceID - gl_VertexID%2);
    vec3 calcPos = vec3(index-vec2(0.5, 0.5)*terrainSize, texture(heights, index/terrainSize).r*35-5);
    wpos = modelMatrix*vec4(calcPos, 1);
  vec4 fpos = viewProjectionMatrix*wpos;
    spos = shadowMatrix*wpos;
  ftex = calcPos.xy*0.1;
  fnorm = vec3(0, 0, 1);
  gl_Position = fpos;
}
