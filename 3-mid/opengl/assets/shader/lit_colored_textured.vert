struct directional_light
{
   vec3   direction;        // Normalized light direction in eye space.
   vec3   halfplane;        // Normalized half-plane vector.
   vec4   ambient_color;     
   vec4   diffuse_color;
   vec4   specular_color;
};


uniform   mat4                  mvp_Matrix;
uniform   mat3                  inv_modelview_Matrix;
uniform   directional_light     uLight_1;
uniform   directional_light     uLight_2;


attribute vec3   aSite;
attribute vec3   aNormal;
attribute vec4   aColor;
attribute vec2   aCoords;


varying   vec4   vColor;
varying   vec2   vCoords;


const float   c_zero      = 0.0;
const float   c_one       = 1.0;
const float   c_shininess = 200.0;



vec4                                           // Returns the computed color.
directional_light_1_color (vec3   normal)      // 'normal' has been transformed into eye space and normalized.
{
   vec4    computed_color = vec4 (c_zero, c_zero, c_zero, c_zero);
   float   NdotL;                              // Dot product of normal and light direction.
   float   NdotH;                              // Dot product of normal and half-plane vector.

   NdotL = max (c_zero,  dot (normal, uLight_1.direction));
   NdotH = max (c_zero,  dot (normal, uLight_1.halfplane));

   computed_color += (        uLight_1.ambient_color * aColor);
   computed_color += (NdotL * uLight_1.diffuse_color * aColor);
   
   if (NdotH > c_zero)
      computed_color += (pow (NdotH, c_shininess) * aColor * uLight_1.specular_color);

   return computed_color;
}


vec4                                           // Returns the computed color.
directional_light_2_color (vec3   normal)      // 'normal' has been transformed into eye space and normalized.
{
   vec4    computed_color = vec4 (c_zero, c_zero, c_zero, c_zero);
   float   NdotL;                              // Dot product of normal and light direction.
   float   NdotH;                              // Dot product of normal and half-plane vector.

   NdotL = max (c_zero,  dot (normal, uLight_2.direction));
   NdotH = max (c_zero,  dot (normal, uLight_2.halfplane));

   computed_color += (        uLight_2.ambient_color * aColor);
   computed_color += (NdotL * uLight_2.diffuse_color * aColor);
   
   if (NdotH > c_zero)
      computed_color += (pow (NdotH, c_shininess) * aColor * uLight_2.specular_color);

   return computed_color;
}



void main()
{
   gl_Position = mvp_Matrix * vec4 (aSite, 1.0);

   vColor  = directional_light_1_color (normalize (aNormal) * inv_modelview_Matrix);
   vColor += directional_light_2_color (normalize (aNormal) * inv_modelview_Matrix);

   vCoords = aCoords;
}
