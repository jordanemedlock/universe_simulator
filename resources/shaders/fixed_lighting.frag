#version 330 core

in vec3 FragPos;
in vec3 Normal;
in vec2 TexCoord;

out vec4 FragColor;

uniform vec4 ambientColor;
uniform vec4 objectColor;

uniform vec3 viewPos;

uniform vec3 lightPos;
uniform vec4 lightColor;

uniform sampler2D objectTexture;

void main()
{
    // result
    // vec4 result = (ambient + diffuse + specular) * texture(objectTexture, TexCoord);
    vec4 result = objectColor * texture(objectTexture, TexCoord);
    if (result.a < 0.5) discard; else FragColor = result;
    FragColor = result;
}