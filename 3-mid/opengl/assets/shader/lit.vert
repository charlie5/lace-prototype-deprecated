#version 150

uniform mat4   camera;
uniform mat4   model;

uniform mat4   mvp_Matrix;
uniform vec3   uScale;

in vec3        vert;
in vec2        vertTexCoord;
in vec3        vertNormal;

in vec4        aColor;
in float       uShine;

out vec3       fragVert;
out vec2       fragTexCoord;
out vec3       fragNormal;

out vec4       frag_Color;
out float      frag_Shine;


void main()
{
    // Pass some variables to the fragment shader.
    //
    frag_Color   = aColor;

    fragTexCoord = vertTexCoord;
    fragNormal   = vertNormal;
    fragVert     = vert;
//    fragVert     = vec3 (vert[0], vert[1], vert[2] + uShine);
//    frag_Shine   = uShine;
    
    // Apply all matrix transformations to 'vert'.
    //
//    gl_Position = camera * model * vec4 (vert * uScale, 1);
    gl_Position = mvp_Matrix * vec4 (vert * uScale, 1);
}