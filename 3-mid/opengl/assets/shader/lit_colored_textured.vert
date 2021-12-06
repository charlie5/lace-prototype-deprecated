#version 120


struct directional_light
{
   vec3   direction;        // Normalized light direction in eye space.
   vec3   halfplane;        // Normalized half-plane vector.

   vec4   ambient_color;     
   vec4   diffuse_color;
   vec4   specular_color;
};


uniform   mat4                  mvp_Transform;
uniform   mat3                  inv_modelview_Matrix;

uniform   vec3                  Scale;
uniform   float                 uShine;

uniform   directional_light     uLights [8];


attribute vec3   aSite;
attribute vec3   aNormal;
attribute vec4   aColor;
attribute vec2   aCoords;


varying   vec4   vColor;
varying   vec2   vCoords;


const float   c_zero      = 0.0;
const float   c_one       = 1.0;



vec4                                                        // Returns the computed color.
directional_light_color (in vec3                normal,     // 'normal' has been transformed into eye space and normalized.
                         in directional_light   light)
{
   vec4    computed_color = vec4 (c_zero, c_zero, c_zero, c_zero);
   float   NdotL;                                            // Dot product of normal and light direction.
   float   NdotH;                                            // Dot product of normal and half-plane vector.

   NdotL = max (c_zero,  dot (normal, light.direction));
   NdotH = max (c_zero,  dot (normal, light.halfplane));

   computed_color += (        light.ambient_color * aColor);
   computed_color += (NdotL * light.diffuse_color  * aColor);
   
   if (NdotH > c_zero)
      computed_color += (pow (NdotH, uShine) * aColor * light.specular_color);

   return computed_color;
}



void main()
{
   gl_Position = mvp_Transform * vec4 (aSite * Scale, 1.0);


   vec3   light_Normal = normalize (aNormal) * inv_modelview_Matrix;


   vColor = vec4 (0.0, 0.0, 0.0, 0.0);

   for (int i = 0; i < 8; i++)
   {
      vColor += directional_light_color (light_Normal, uLights [i]);
   }

   vCoords = aCoords;
}
