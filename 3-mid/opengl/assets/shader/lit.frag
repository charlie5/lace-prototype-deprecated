#version 150

struct Light
{
   vec4    position;
   vec3    intensities;     // The color of the light.
   float   attenuation;
   float   ambientCoefficient;
   float   coneAngle;
   vec3    coneDirection;
};


uniform mat4           model;
uniform vec3           cameraPosition;
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
    
    if (light.position.w == 0.0)
    {
        // Directional light.
        //
        surfaceToLight = normalize (light.position.xyz);
        attenuation    = 1.0;     // No attenuation for directional lights.
    } 
    else
    {
        // Point light.
        //
        float   distanceToLight = length (light.position.xyz - surfacePos);

        surfaceToLight = normalize (light.position.xyz - surfacePos);
        attenuation    =   1.0
                         / (  1.0 
                            +   light.attenuation
                              * pow (distanceToLight, 2));

        // Cone restrictions which affects attenuation.
        //
        float   lightToSurfaceAngle = degrees (acos (dot (-surfaceToLight, 
                                                          normalize (light.coneDirection))));
        
        if (lightToSurfaceAngle > light.coneAngle)
        {
            attenuation = 0.0;
        }
    }

    vec3    ambient             = light.ambientCoefficient * surfaceColor.rgb * light.intensities;
    float   diffuseCoefficient  = max (0.0, 
                                       dot (normal,
                                            surfaceToLight));                   
    vec3    diffuse             = diffuseCoefficient * surfaceColor.rgb * light.intensities;
    float   specularCoefficient = 0.0;

    if (diffuseCoefficient > 0.0)
        specularCoefficient = pow (max (0.0, 
                                        dot (surfaceToCamera, 
                                             reflect (-surfaceToLight,
                                                      normal))),
                                   frag_Shine);

    vec3   specular = specularCoefficient * materialSpecularColor * light.intensities;

    return ambient + attenuation * (diffuse + specular);     // Linear color (before gamma correction).
}



void
main()
{
    vec3   surfacePos      = vec3 (  model
                                   * vec4 (frag_Site, 1));
                                   
    vec4   surfaceColor    = (  texture  (materialTex,
                                          frag_Coord)
                              + frag_Color)
                              / 2.0;

    vec3   surfaceToCamera = normalize (cameraPosition - surfacePos);
    vec3   normal          = normalize (  transpose (inverse (mat3 (model)))
                                        * frag_Normal);

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