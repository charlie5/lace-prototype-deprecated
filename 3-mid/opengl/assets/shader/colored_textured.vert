#version 120

uniform   mat4   mvp_Transform;
uniform   vec3   Scale;

attribute vec3   Site;
attribute vec4   Color;
attribute vec2   Coords;

varying   vec4   vColor;
varying   vec2   vCoords;


void main()
{
   gl_Position = mvp_Transform * vec4 (Site * Scale, 1.0);
   vColor      = Color;
   vCoords     = Coords;
}
