#version 120

uniform   mat4   mvp_Matrix;
uniform   vec3   Scale;

attribute vec3   aSite;
attribute vec4   aColor;

varying   vec4   vColor;


void main()
{
   gl_Position = mvp_Matrix * vec4 (aSite * Scale, 1.0);
   vColor      = aColor;
}
