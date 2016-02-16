#version 330 core

uniform sampler2D material, material2;
uniform sampler2DShadow shadow;
uniform vec3 sunPos;
uniform float lightOverride;

in vec4 spos, wpos;
in vec3 fnorm;
in vec2 ftex;

out vec3 color;

const vec3 SunLight = {1, 1, 0.9};
const vec3 MoonLight = {0.25, 0.25, 0.35};

void main(){
  if (wpos.z < 0.5)
    discard;
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
    if (sposb.x < 0.001 || sposb.x > 0.999 || sposb.y < 0.001 || sposb.y > 0.999)
      shadowZ = 1;
    else
      shadowZ = texture(shadow, sposb);

    float brightness = dot(fnorm, smPos);
    if (brightness < 0)
      brightness = 0;
    else
      brightness = brightness * shadowZ;
    if (fnorm.z > 0.95 && wpos.z > 2)
      color = texture(material, ftex).rgb * (smLight * brightness + ambient);
    else
      color = texture(material2, ftex).rgb * (smLight * brightness + ambient);
//    color = abs(fnorm);
  }
}
