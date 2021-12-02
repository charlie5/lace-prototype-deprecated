#version 150

uniform mat4   model_Matrix;
uniform mat4   mvp_Matrix;
uniform vec3   Scale;

in vec3        Site;
in vec3        Normal;
in vec4        Color;
in vec2        Coord;
in float       Shine;

out mat3       inverse_Model_matrix;
out vec3       frag_Site;
out vec3       frag_Normal;
out vec4       frag_Color;
out vec2       frag_Coord;
out float      frag_Shine;


void main()
{
    // Pass some variables to the fragment shader.
    //
    inverse_Model_matrix = inverse (mat3 (model_Matrix));
    
    frag_Site   = Site;
    frag_Normal = Normal;
    frag_Color  = Color;
    frag_Coord  = Coord;
    frag_Shine  = Shine;
    
    // Apply all matrix transformations to 'Site'.
    //
    gl_Position = mvp_Matrix * vec4 (Site * Scale, 1);
}