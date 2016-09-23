#version 330 core

uniform   mat4   mvp_Matrix;

attribute vec3   aSite;
attribute vec4   aColor;
attribute vec2   aCoords;

varying   vec4   vColor;
varying   vec2   vCoords;


void main()
{
   gl_Position = mvp_Matrix * vec4 (aSite, 1.0);
   vColor      = aColor;
   vCoords     = aCoords;
}
