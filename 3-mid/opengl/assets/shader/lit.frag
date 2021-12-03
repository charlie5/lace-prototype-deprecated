#version 150

struct Light
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
uniform vec3           materialSpecularColor;
uniform sampler2D      materialTex;
uniform int            numLights;
uniform struct Light   uLights [10];


in  vec3   frag_Site;
in  vec3   frag_Normal;
in  vec4   frag_Color;
in  vec2   frag_Coord;
in  float  frag_Shine;

out vec4   finalColor;


vec3
ApplyLight (Light   light, 
            vec3    surfaceColor, 
            vec3    normal, 
            vec3    surfacePos, 
            vec3    surfaceToCamera)
{
    vec3    surfaceToLight;
    float   attenuation = 1.0;
    
    if (light.Site.w == 0.0)
    {
        // Directional light.
        //
        surfaceToLight = normalize (light.Site.xyz);
        attenuation    = 1.0;     // No attenuation for directional lights.
    } 
    else
    {
        // Point light.
        //
        vec3    Surface_to_Light_vector = light.Site.xyz - surfacePos;
        float   distanceToLight         = length (Surface_to_Light_vector);

        surfaceToLight = normalize (Surface_to_Light_vector);
        attenuation    =   1.0
                         / (  1.0 
                            +   light.Attenuation
                              * pow (distanceToLight, 2));

        // Cone restrictions which affects attenuation.
        //
        float   lightToSurfaceAngle = degrees (acos (dot (-surfaceToLight, 
                                                          normalize (light.cone_Direction))));
        
        if (lightToSurfaceAngle > light.cone_Angle)
        {
            attenuation = 0.0;
        }
    }

    vec3    lit_surface_Color   = surfaceColor * light.Color;
    vec3    ambient             = light.ambient_Coefficient * lit_surface_Color;
    float   diffuseCoefficient  = max (0.0, 
                                       dot (normal,
                                            surfaceToLight));                   
    vec3    diffuse             = diffuseCoefficient * lit_surface_Color;
    float   specularCoefficient = 0.0;

    if (diffuseCoefficient > 0.0)
        specularCoefficient = pow (max (0.0, 
                                        dot (surfaceToCamera, 
                                             reflect (-surfaceToLight,
                                                      normal))),
                                   frag_Shine);

    vec3   specular = specularCoefficient * materialSpecularColor * light.Color;

    return ambient + attenuation * (diffuse + specular);     // Linear color (before gamma correction).
}



void
main()
{
    vec3   surfacePos      = vec3 (  model_Transform
                                   * vec4 (frag_Site, 1));
                                   
    vec4   surfaceColor    =    (   texture  (materialTex, frag_Coord)
                                  + frag_Color)
                              / 2.0;

    vec3   surfaceToCamera = normalize (camera_Site - surfacePos);
    vec3   normal          = normalize (  frag_Normal
                                        * inverse_model_Rotation);

    // Combine color from all the lights.
    //
    vec3   linearColor = vec3 (0);
    
    for (int i = 0;   i < numLights;   ++i)
    {
        linearColor += ApplyLight (uLights [i],
                                   surfaceColor.rgb,
                                   normal,
                                   surfacePos,
                                   surfaceToCamera);
    }
    
    vec3   gamma = vec3 (1.0 / 2.2);

    finalColor = vec4 (pow (linearColor,     // Final color (after gamma correction).
                            gamma),
                       surfaceColor.a);
}