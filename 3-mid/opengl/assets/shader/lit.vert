#version 150

uniform mat4   mvp_Matrix;
uniform vec3   uScale;

in vec3        vert;
in vec2        vertTexCoord;
in vec3        vertNormal;
in vec4        aColor;
in float       Shine;

out vec3       fragVert;
out vec2       fragTexCoord;
out vec3       fragNormal;
out vec4       frag_Color;
out float      vert_Shine;


void main()
{
    // Pass some variables to the fragment shader.
    //
    frag_Color   = aColor;
    fragTexCoord = vertTexCoord;
    fragNormal   = vertNormal;
    fragVert     = vert;
    vert_Shine   = Shine;
    
    // Apply all matrix transformations to 'vert'.
    //
    gl_Position = mvp_Matrix * vec4 (vert * uScale, 1);
}