#version 330 core

uniform mat4 modelMatrix, viewProjectionMatrix, shadowMatrix;
uniform sampler2D heights;

uniform float worldSize, terrainDetail;
uniform vec3 cameraPos;

out vec4 spos, wpos;
out vec2 ftex;

#define PI 3.1415926535897932384626433832795

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
  return (255-texture(heights, texCoords).w*255)*0.5;
}

vec3 vpos1(int vertexID, int instanceID) {
  vec2 index = vec2(vertexID-(vertexID%1+instanceID%2)*1, instanceID-vertexID%2);
  index = vec2(index)-vec2(0.5, 0.5)*terrainDetail;
  float n = max(abs(index.x), abs(index.y));
  if (n != 0) {
    float r = n*sqrt(2), a = (index.y-index.x+n)*PI/(4*n);
    r *= 0.1*r;
    if (index.x == -n || index.y == -n)
      a = 1.5*PI - a;
    index = vec2(r*cos(a), r*sin(a));
    index += cameraPos.xy;
  }
  return vec3(index, getHeight(vec2(0.5, 0.5)+index/worldSize)*(-255)+255);
}

const vec2 dir[4] = {vec2(1,1),vec2(-1,1),vec2(-1,-1),vec2(1,-1)};
vec3 vpos2(int vertexID, int instanceID) {
  float v = (vertexID+vertexID%2)/2;
  v *= v*0.1;
  vec2 index;
  if (vertexID%2==1)
    index = v*dir[instanceID];
  else
    index = v*dir[(instanceID+1)%4];
  return vec3(index, getHeight(vec2(0.5, 0.5)+index/worldSize)*(-255)+255);
}

vec2 vpos3(int vertexID, int instanceID) {
  vec2 index = vec2((vertexID-vertexID%2)/2, instanceID - vertexID%2);
  index = index-vec2(0.5, 0.5)*terrainDetail;
//  vec2 mag = abs((2*index/terrainDetail/terrainDetail)*worldSize);
//  index *= max(vec2(1, 1), mag);
//  index += round(cameraPos.xy);
  return index;//vec3());
}

void main() {
  wpos = vec4(vpos3(gl_VertexID, gl_InstanceID), 0, 1);
  wpos = modelMatrix*wpos;
  wpos.xy += cameraPos.xy;
  wpos.z = cameraPos.z + getHeight(vec2(0.5, 0.5)+wpos.xy/worldSize);
  vec4 fpos = viewProjectionMatrix*wpos;
  spos = shadowMatrix*wpos;
  ftex = wpos.xy*0.1;
  gl_Position = fpos;
}
