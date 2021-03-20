#version 330 core
in vec2 texCoords;
out vec4 color;

uniform vec4 spriteColor;
uniform sampler2D image;

void main()
{
    vec4 texColor = texture(image, texCoords);
    color = spriteColor * texColor;
}