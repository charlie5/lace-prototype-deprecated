with
     openGL;

package gel.Conversions
is

   function to_GL   (Self : in math.Real)                  return opengl.Real;
   function to_GL   (Self : in math.Vector_3)              return opengl.Vector_3;
   function to_GL   (Self : in math.Matrix_3x3)            return opengl.Matrix_3x3;
   function to_GL   (Self : in math.Matrix_4x4)            return opengl.Matrix_4x4;
   function to_GL   (Self : in geometry_3d.bounding_Box)   return opengl.Bounds;

   function to_Math (Self : in opengl.Vector_3)            return math.Vector_3;

end gel.Conversions;
