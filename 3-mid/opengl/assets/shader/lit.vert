#version 150

uniform mat4   mvp_Matrix;
uniform vec3   uScale;

in vec3        Site;
in vec3        Normal;
in vec4        Color;
in vec2        Coord;
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
    fragVert     = Site;
    fragNormal   = Normal;
    frag_Color   = Color;
    fragTexCoord = Coord;
    vert_Shine   = Shine;
    
    // Apply all matrix transformations to 'Site'.
    //
    gl_Position = mvp_Matrix * vec4 (Site * uScale, 1);
}