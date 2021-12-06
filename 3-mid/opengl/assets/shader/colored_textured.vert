#version 120

uniform   mat4   mvp_Transform;
uniform   vec3   Scale;

attribute vec3   aSite;
attribute vec4   aColor;
attribute vec2   aCoords;

varying   vec4   vColor;
varying   vec2   vCoords;


void main()
{
   gl_Position = mvp_Transform * vec4 (aSite * Scale, 1.0);
   vColor      = aColor;
   vCoords     = aCoords;
}
