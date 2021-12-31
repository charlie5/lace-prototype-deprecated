#version 120

uniform   mat4   mvp_Transform;
uniform   vec3   Scale;


attribute vec3   Site;
attribute vec2   Coords;


varying   vec4   vColor;
varying   vec2   vCoords;


const float      c_zero = 0.0;
const float      c_one  = 1.0;


void main()
{
   gl_Position = mvp_Transform * vec4 (Site * Scale, 1.0);
   
   vColor  = vec4 (1.0, 1.0, 1.0, 1.0);
   vCoords = Coords;
}
