#version 330
in vec2 in_Position;
in vec4 in_Color;

out vec4 ex_Color;

void main(void) {
    // Translate from 2d plane to 4d coords: z=0, w=1
    gl_Position = vec4(in_Position.x, in_Position.y, 0.0, 1.0);

    // Color passes through unmodified
    ex_Color = in_Color;
}