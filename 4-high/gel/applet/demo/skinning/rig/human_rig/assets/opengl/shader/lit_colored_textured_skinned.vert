#version 150

uniform mat4   mvp_Transform;
uniform vec3   Scale;
uniform mat4   bone_Matrices[120];

in vec3        Site;
in vec3        Normal;
in vec4        Color;
in vec2        Coords;
in float       Shine;
in vec4        bone_Ids;
in vec4        bone_Weights;

out vec3       frag_Site;
out vec3       frag_Normal;
out vec4       frag_Color;
out vec2       frag_Coords;
out float      frag_Shine;

const float    c_zero = 0.0;
const float    c_one  = 1.0;


void main()
{
    vec4   transformedPosition = vec4 (0.0);
    vec3   transformedNormal   = vec3 (0.0);

    if (int (bone_Ids.x) == 0)     // No bones affect this vertex.
    {
        transformedPosition = vec4 (Site, c_one);
        transformedNormal   = Normal;
    }
    else
    {
        // Bone 1.
        //
        mat4   m44 = bone_Matrices [int (bone_Ids.x) - 1];
        
        // Transform the offset by bone 1.
        transformedPosition += m44 * vec4 (Site, c_one) * bone_Weights.x;

        mat3   m33 = mat3 (m44[0].xyz,
                           m44[1].xyz,
                           m44[2].xyz);

        // Transform the normal by bone 1.
        transformedNormal += m33 * Normal * bone_Weights.x;

        if (int (bone_Ids.y) != 0)
        {
            // Bone 2.
            //
            m44 = bone_Matrices [int (bone_Ids.y) - 1];
        
            // Transform the offset by bone 2.
            transformedPosition += m44 * vec4 (Site, c_one) * bone_Weights.y;

            m33 = mat3 (m44[0].xyz,
                        m44[1].xyz,
                        m44[2].xyz);

            // Transform the normal by bone 2.
            transformedNormal += m33 * Normal * bone_Weights.y;

            if (int (bone_Ids.z) != 0)
            {
                // Bone 3.
                //
                m44 = bone_Matrices [int (bone_Ids.z) - 1];
        
                // Transform the offset by bone 3.
                transformedPosition += m44 * vec4 (Site, c_one) * bone_Weights.z;

                m33 = mat3(m44[0].xyz,
                           m44[1].xyz,
                           m44[2].xyz);

                // Transform the normal by bone 3.
                transformedNormal += m33 * Normal * bone_Weights.z;

                if (int (bone_Ids.w) != 0)
                {
                    // Bone 4.
                    //
                    m44 = bone_Matrices [int (bone_Ids.w) - 1];
        
                    // Transform the offset by bone 4.
                    transformedPosition += m44 * vec4 (Site, c_one) * bone_Weights.w;

                    m33 = mat3 (m44[0].xyz,
                                m44[1].xyz,
                                m44[2].xyz);

                    // Transform the normal by bone 4.
                    transformedNormal += m33 * Normal * bone_Weights.w;
                }
            }
        }
    }

    // Pass some variables to the fragment shader.
    //
    frag_Site   = Site;
    frag_Normal = normalize (transformedNormal);
    frag_Color  = Color;
    frag_Coords = Coords;
    frag_Shine  = Shine;
    
    // Apply all matrix transformations to 'Site'.
    //
    gl_Position = mvp_Transform * (transformedPosition * vec4 (Scale, 1));
}
