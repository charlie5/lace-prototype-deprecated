#version 150

uniform mat4        model;
uniform vec3        cameraPosition;

uniform sampler2D   materialTex;
//uniform float       uShine;     // materialShininess;
//uniform   float       frag_Shine;
uniform vec3        materialSpecularColor;

#define MAX_LIGHTS  10

uniform int         numLights;

uniform struct Light
{
   bool    is_on;
   vec4    position;
   vec3    intensities;     // The color of the light.
   float   attenuation;
   float   ambientCoefficient;
   float   coneAngle;
   vec3    coneDirection;
}                   uLights [MAX_LIGHTS];

in vec2             fragTexCoord;
in vec3             fragNormal;
in vec3             fragVert;

in vec4             frag_Color;
in float            frag_Shine;
in float            uShine;


out vec4            finalColor;


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
                                   //frag_Shine);
                                   uShine);
                                   // materialShininess);

    vec3 specular = specularCoefficient * materialSpecularColor * light.intensities;

    return ambient + attenuation * (diffuse + specular);     // Linear color (before gamma correction).
}


void
main()
{
    vec3   surfacePos      = vec3 (  model
                                   * vec4 (fragVert, 1));
                                   
    vec4   surfaceColor    = (  texture  (materialTex,
                                          fragTexCoord)
                              + frag_Color)
                              / 2.0;
    vec3   surfaceToCamera = normalize (cameraPosition - surfacePos);
    vec3   normal          = normalize (  transpose (inverse (mat3 (model)))
                                        * fragNormal);

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