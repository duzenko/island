#version 330 core

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

const vec2 data[4] = vec2[]
(
  vec2(-1.0,  1.0),
  vec2(-1.0, -1.0),
  vec2( 1.0,  1.0),
  vec2( 1.0, -1.0)
);

void main() {
  for (int i = 0; i < 4; ++i) {
    gl_Position = gl_in[0].gl_Position + vec4(data[i], 0.0, 1.0);
    EmitVertex();
  }
  EndPrimitive();
}
