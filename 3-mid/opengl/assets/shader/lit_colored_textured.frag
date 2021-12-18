#version 150

struct light
{
   vec4    Site;
   vec3    Color;
   float   Attenuation;
   float   ambient_Coefficient;
   float   cone_Angle;
   vec3    cone_Direction;
};


uniform mat4           model_Transform;
uniform mat3           inverse_model_Rotation;
uniform vec3           camera_Site;
uniform vec3           specular_Color;    // The materials specular color.
uniform sampler2D      Texture;
uniform int            light_Count;
uniform struct light   Lights [10];


in  vec3   frag_Site;
in  vec3   frag_Normal;
in  vec4   frag_Color;
in  vec2   frag_Coords;
in  float  frag_Shine;

out vec4   final_Color;


vec3
apply_Light (light   Light, 
             vec3    surface_Color, 
             vec3    Normal, 
             vec3    surface_Site, 
             vec3    Surface_to_Camera)
{
    vec3    Surface_to_Light;
    float   Attenuation = 1.0;
    
    if (Light.Site.w == 0.0)
    {
        // Directional light.
        //
        Surface_to_Light = normalize (Light.Site.xyz);
        Attenuation      = 1.0;     // No attenuation for directional lights.
    } 
    else
    {
        // Point light.
        //
        vec3    Surface_to_Light_vector = Light.Site.xyz - surface_Site;
        float   Distance_to_Light       = length (Surface_to_Light_vector);

        Surface_to_Light = normalize (Surface_to_Light_vector);
        Attenuation      =   1.0
                           / (  1.0 
                              +   Light.Attenuation
                                * pow (Distance_to_Light, 2));

        // Cone restrictions which affects attenuation.
        //
        float   Light_to_Surface_Angle = degrees (acos (dot (-Surface_to_Light, 
                                                             normalize (Light.cone_Direction))));
        
        if (Light_to_Surface_Angle > Light.cone_Angle)
        {
            Attenuation = 0.0;
        }
    }

    vec3    lit_surface_Color    = surface_Color * Light.Color;
    vec3    Ambient              = Light.ambient_Coefficient * lit_surface_Color;
    float   diffuse_Coefficient  = max (0.0, 
                                        dot (Normal,
                                             Surface_to_Light));                   
    vec3    Diffuse              = diffuse_Coefficient * lit_surface_Color;
    float   specular_Coefficient = 0.0;

    if (diffuse_Coefficient > 0.0)
        specular_Coefficient = pow (max (0.0, 
                                         dot (Surface_to_Camera, 
                                              reflect (-Surface_to_Light,
                                                       Normal))),
                                    frag_Shine);

    vec3   Specular = specular_Coefficient * specular_Color * Light.Color;

    return Ambient + Attenuation * (Diffuse + Specular);     // Linear color (before gamma correction).
}



void
main()
{
    vec3   surface_Site      = vec3 (  model_Transform
                                     * vec4 (frag_Site, 1));
                                   
    vec4   surface_Color     = mix (texture (Texture, frag_Coords),
                                    frag_Color,
                                    0.5);

    vec3   Surface_to_Camera = normalize (camera_Site - surface_Site);
    vec3   Normal            = normalize (frag_Normal * inverse_model_Rotation);

    // Combine color from all the lights.
    //
    vec3   linear_Color = vec3 (0);
    
    for (int i = 0;   i < light_Count;   ++i)
    {
        linear_Color += apply_Light (Lights [i],
                                     surface_Color.rgb,
                                     Normal,
                                     surface_Site,
                                     Surface_to_Camera);
    }
    
    vec3   Gamma = vec3 (1.0 / 2.2);

    final_Color = vec4 (pow (linear_Color,     // Final color (after gamma correction).
                             Gamma),
                        surface_Color.a);
}