#version 330 core

uniform mat4 modelMatrix, viewProjectionMatrix, shadowMatrix;
uniform sampler2D heights;

uniform float worldSize, visibility;

out vec4 spos, wpos;
flat out vec3 fnorm;
out vec2 ftex;

vec3 calcNormal(vec3 v, vec3 w) {
  vec3 n = normalize((cross(v, w)));
  if (n.z < 0)
    return -n;
  else
    return n;
}

vec3 vposFor(int vertexID, int instanceID) {
  vec2 index = vec2((vertexID-vertexID%2)/2, instanceID - vertexID%2);
  index = index-vec2(0.5, 0.5)*visibility;
  return vec3(index,
    texture(heights, vec2(0.5, 0.5)+index/worldSize).r*(-255)+255);
}

void main() {
  vec3 vpos = vposFor(gl_VertexID, gl_InstanceID),
    neighbor1=vposFor(gl_VertexID-1, gl_InstanceID),
    neighbor2=vposFor(gl_VertexID-2, gl_InstanceID);
  wpos = modelMatrix*vec4(vpos, 1);
  vec4 fpos = viewProjectionMatrix*wpos;
  spos = shadowMatrix*wpos;
  ftex = vpos.xy*0.1;
  fnorm = calcNormal(neighbor2-vpos, neighbor1-vpos);
  gl_Position = fpos;
}
