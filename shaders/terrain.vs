#version 330 core

uniform mat4 modelMatrix, viewProjectionMatrix, shadowMatrix;
uniform sampler2D heights;

uniform float worldSize, terrainDetail;

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

float getHeight(vec2 texCoords) {
/*    vec2 i1 := floor(texCoords), i2=i1+vec2(1, 1);
    iy := Floor(y);
    h[0, 0] := HeightAt(ix, iy);
    h[0, 1] := HeightAt(ix, iy+1);
    h[1, 0] := HeightAt(ix+1, iy);
    h[1, 1] := HeightAt(ix+1, iy+1);
    Result := h[0, 0]*(ix+1-x)*(iy+1-y)+h[1, 0]*(x-ix)*(iy+1-y)+h[0, 1]*(ix+1-x)*(y-iy)+h[1, 1]*(x-ix)*(y-iy);*/
  return texture(heights, texCoords).r;
}

vec3 vposFor(int vertexID, int instanceID) {
  vec2 index = vec2((vertexID-vertexID%2)/2, instanceID - vertexID%2);
  index = vec2(index)-vec2(0.5, 0.5)*terrainDetail;
  vec2 mag = abs((2*index/terrainDetail/terrainDetail)*worldSize);
//  index *= max(vec2(1, 1), mag);
  return vec3(index, getHeight(vec2(0.5, 0.5)+index/worldSize)*(-255)+255);
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
