#version 330 core
in vec2 texCoords;
out vec4 color;

uniform vec4 spriteColor;
uniform sampler2D image;
uniform vec2 dimensions;
uniform vec2 borders;

// Helper function, because WET code is bad code
// Takes in the coordinate on the current axis and the borders 
float processAxis(float coord, float textureBorder, float windowBorder) {
    if (coord < windowBorder)
        return map(coord, 0, windowBorder, 0, textureBorder) ;
    if (coord < 1 - windowBorder) 
        return map(coord,  windowBorder, 1 - windowBorder, textureBorder, 1 - textureBorder);
    return map(coord, 1 - windowBorder, 1, 1 - textureBorder, 1);
} 

float map(float value, float originalMin, float originalMax, float newMin, float newMax) {
    return (value - originalMin) / (originalMax - originalMin) * (newMax - newMin) + newMin;
}

void main()
{
    vec2 newUV = vec2(
        processAxis(texCoords.x, border.x, dimensions.x),
        processAxis(texCoords.y, border.y, dimensions.y)
    );
    // Output the color
    color = spriteColor * texture2D(image, newUV);
}


