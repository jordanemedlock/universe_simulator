
#version 330 core

in vec3 FragPos;
in vec3 Normal;

out vec4 FragColor;

uniform vec4 ambientColor;
uniform vec4 objectColor;

uniform vec3 viewPos;

uniform vec3 lightPos;
uniform vec4 lightColor;

void main()
{
    // constants
    float ambientStrength = 0.1;
    float specularStrength = 0.5;

    // diffuse
    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(lightPos - FragPos);  
    float diff = max(dot(norm, lightDir), 0.0);
    vec4 diffuse = diff * lightColor;

    // ambient
    vec4 ambient = ambientStrength * ambientColor;

    // specular
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    vec4 specular = specularStrength * spec * lightColor;

    // result
    vec4 result = (ambient + diffuse + specular) * objectColor;
    FragColor = result;
}