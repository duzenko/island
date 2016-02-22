#version 330 core

uniform sampler2D material, material2, heights;
uniform sampler2DShadow shadow;

uniform vec3 sunPos;
uniform float lightOverride;
uniform float worldSize;

in vec4 spos, wpos;
//in vec3 fnormt, fnormv;
in vec2 ftex;

out vec3 color;

const vec3 SunLight = {1, 1, 0.9};
const vec3 MoonLight = {0.25, 0.25, 0.35};

void main(){
  if (wpos.z < 0.5)
    discard;
  vec3 fnorm;
  fnorm = texture(heights, vec2(0.5, 0.5)+wpos.xy/worldSize).rgb - vec3(0.5, 0.5, 0.5);
  fnorm *= vec3(-2, 2, 2);

  float mz = 0.1*(sunPos.z+2);
  vec3 smPos, smLight, ambient = vec3(mz, mz, 1.3*mz);
  if (sunPos.z < 0) {
    smPos = -sunPos;
    smLight = MoonLight*min(1, 22*smPos.z*smPos.z);
  } else {
    smPos = sunPos;
    smLight = SunLight*min(1, 22*smPos.z*smPos.z);
  }

  if (lightOverride == 1)
    color = texture(material, ftex).rgb;
  else {
    vec3 sposb = spos.xyz*0.5+vec3(0.5, 0.5, 0.5);
    float shadowZ;
    if (sposb.x < 0.001 || sposb.x > 0.999 || sposb.y < 0.001 || sposb.y > 0.999 || sposb.z < 0.001 || sposb.z > 0.999 )
      shadowZ = 1;
    else
//      shadowZ = texture(shadow, sposb.xy).z-sposb.z;
      shadowZ = texture(shadow, sposb);

    float brightness = 0;
    if (shadowZ<=0)
      brightness = 0;
    else
      brightness = dot(fnorm, smPos);// * normalize(shadowZ);
    if (fnorm.z > 0.95 && wpos.z > 2)
      color = texture(material, ftex).rgb * (smLight * brightness + ambient);
    else
      color = texture(material2, ftex).rgb * (smLight * brightness + ambient);
//    color = fract(vec3(texture(shadow, sposb.xy).z, 0, sposb.z));
//    color = vec3(texture(shadow, sposb.xy).z-sposb.z, 0, sposb.z-texture(shadow, sposb.xy).z);
  }
}
