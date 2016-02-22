#version 330 core

uniform mat4 modelMatrix, viewProjectionMatrix, shadowMatrix;
//uniform sampler2D heights;
uniform float instanced;

in vec3 vpos, vnorm, instPos;
in vec2 vtex;

out vec4 spos;
out vec3 fnorm;
out vec2 ftex;

void main() {
  vec4 fpos;
    ftex = vtex;
    fnorm = normalize(modelMatrix*vec4(vnorm, 0)).xyz;
    vec4 wpos = modelMatrix*vec4(vpos, 1);
    if (instanced==1)
      fpos = viewProjectionMatrix*(wpos+vec4(instPos, 0));
    else
      fpos = viewProjectionMatrix*(wpos);
    spos = shadowMatrix*modelMatrix*vec4(vpos, 1);
  gl_Position = fpos;
}
