#version 330 core

uniform mat4 modelMatrix, viewProjectionMatrix, shadowMatrix;

in vec3 vpos, vnorm;
in vec2 vtex;

out vec4 spos;
out vec3 fnorm;
out vec2 ftex;

void main() {
  vec4 fpos;
    ftex = vtex;
    fnorm = normalize(modelMatrix*vec4(vnorm, 0)).xyz;
    fpos = viewProjectionMatrix*modelMatrix*vec4(vpos, 1);
    spos = shadowMatrix*modelMatrix*vec4(vpos, 1);
  gl_Position = fpos;
}
