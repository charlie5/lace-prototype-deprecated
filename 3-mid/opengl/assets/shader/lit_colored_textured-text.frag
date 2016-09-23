#version 330 core

//precision mediump float;

uniform sampler2D   sTexture; 

varying vec4        vColor;
varying vec2        vCoords;



void main()
{
   float    the_Alpha = texture2D (sTexture, vCoords).a;

   if (the_Alpha < 0.2)
   {
      discard;
   }   



   vec4    the_Color = vColor;

   the_Color.a = the_Alpha * vColor.a;    // Modulate color alpha with texture alpha.
   
   
   
   gl_FragColor = the_Color;
}

