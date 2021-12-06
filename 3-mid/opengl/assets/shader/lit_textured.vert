#version 150

uniform mat4   mvp_Transform;
uniform vec3   Scale;

in vec3        Site;
in vec3        Normal;
in vec2        Coords;
in float       Shine;

out vec3       frag_Site;
out vec3       frag_Normal;
out vec2       frag_Coords;
out float      frag_Shine;


void main()
{
    // Pass some variables to the fragment shader.
    //
    frag_Site   = Site;
    frag_Normal = Normal;
    frag_Coords = Coords;
    frag_Shine  = Shine;
    
    // Apply all matrix transformations to 'Site'.
    //
    gl_Position = mvp_Transform * vec4 (Site * Scale, 1);
}