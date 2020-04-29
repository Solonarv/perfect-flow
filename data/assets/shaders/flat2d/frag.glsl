#version 330
precision highp float

in vec4 ex_Color;
out vec4 gl_FragColor;

void main(void) {
    // Pass through original color unmodified
    gl_FragColor = vec4(ex_Color, 1.0)
}